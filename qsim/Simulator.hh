#ifndef SIMULATOR_HH_
#define SIMULATOR_HH_

#include "QueueNetwork.hh"
#include "SplitKernel.hh"
#include "TraceKernel.hh"
#include "PriorityQueue.hh"
#include "Observer.hh"
#include "qsim.h"

#include <algorithm>

#define BRAM_BYTES ((512 * 72) / 8)

class Simulator : public Observer
{
public:

    Simulator() : m_bram_count(0)
    {
    }

    virtual ~Simulator()
    {
        for(size_t i = 0; i < m_kernels.size(); i++) {
            delete m_kernels[i];
        }
    }

    void SetBRAMCount(const uint32_t bram_count)
    {
        m_bram_count = bram_count;
    }

    void AddSplit(const uint32_t in,
                  const uint32_t out0,
                  const uint32_t out1)
    {
        SplitKernel * const k = new SplitKernel(&m_network, in, out0, out1);
        m_kernels.push_back(k);
    }

    void AddTrace(const std::vector<uint32_t> &data, const bool last)
    {
        TraceKernel * const k = new TraceKernel(&m_network, last, data);
        m_kernels.push_back(k);
    }

    void AddQueue(const uint32_t id, const uint32_t word_size)
    {
        Queue * const q = new Queue(id, word_size);
        m_network.AddQueue(id, q);
        m_network.SetDepth(id, 1);
        m_queues.push_back(id);
    }

    uint64_t Run()
    {

        uint32_t bram_count = m_bram_count;

        // Start with all queues set to 1 deep.
        ResetQueueDepths();
        uint64_t best_time = Simulate();

        // While there are BRAMs available, assign them to
        // the queue that provides greatest benefit.
        while(bram_count > 0) {

            // Get the bottleneck.
            uint32_t bottleneck = 0;
            uint64_t most_blocked_time = 0;
            for(size_t i = 0; i < m_queues.size(); i++) {
                const uint32_t qid = m_queues[i];
                const uint64_t blocked_time = m_network.GetBlockedTime(qid);
                if(blocked_time > most_blocked_time) {
                    most_blocked_time = blocked_time;
                    bottleneck = qid;
                }
            }

            // Increase the size of the bottleneck queue
            // and simulate again.
            bram_count -= IncreaseQueueDepth(bottleneck, bram_count);
            const uint64_t t = Simulate();

            // Exit if there was no improvement.
            if(t >= best_time) {
                break;
            }
            best_time = t;

        }

        return best_time;

    }

    std::vector<std::pair<uint32_t, uint32_t> > GetDepths() const
    {
        return m_network.GetDepths();
    }

    virtual void Notify(void *arg)
    {
        m_pq->Push(0, reinterpret_cast<Kernel*>(arg));
    }

private:

    void ResetQueueDepths()
    {
        for(size_t i = 0; i < m_queues.size(); i++) {
            const uint32_t qid = m_queues[i];
            m_network.SetDepth(qid, 1);
        }
    }

    uint32_t IncreaseQueueDepth(const uint32_t qid,
                                const uint32_t bram_count)
    {
        const uint32_t word_size = m_network.GetWordSize(qid);
        const uint32_t old_depth = m_network.GetDepth(qid);
        const uint32_t base_depth = std::max(uint32_t(2),
                                             BRAM_BYTES / word_size);
        const uint32_t max_inc = (bram_count * BRAM_BYTES) / word_size;
        const uint32_t max_depth = old_depth + max_inc;
        if(old_depth == 1) {
            m_network.SetDepth(qid, base_depth);
            return 1;
        } else {
            const uint32_t new_depth = std::min(old_depth * 2, max_depth);
            const uint32_t inc = new_depth - old_depth;
            m_network.SetDepth(qid, new_depth);
            return (inc + base_depth - 1) / base_depth;
        }
    }

    uint64_t Simulate()
    {
        const size_t kernel_count = m_kernels.size();
        m_pq = new PriorityQueue<uint64_t, Kernel*>(kernel_count);
        for(size_t i = 0; i < kernel_count; i++) {
            Kernel * const k = m_kernels[i];
            k->Reset();
            m_pq->Push(0, k);
        }
        m_network.Reset();
        uint64_t t = 1;
        try {
            for(;;) {
                Kernel * const k = m_pq->GetValue();
                t = std::max(t, m_pq->GetKey());
                m_pq->Pop();
                const int64_t next_t = k->Process(t);
                if(next_t >= 0) {
                    m_pq->Push(next_t, k);
                } else {
                    uint32_t i = 0;
                    while(const uint32_t channel = k->GetBlockingChannel(i)) {
                        m_network.RegisterNotification(channel, this, k);
                        i += 1;
                    }
                    assert(i > 0);
                }
            }
        } catch(const EndSimulation &end) {
            t = end.GetTime();
        }
        delete m_pq;
        return t;
    }

    QueueNetwork m_network;
    std::vector<Kernel*> m_kernels;
    std::vector<uint32_t> m_queues;
    PriorityQueue<uint64_t, Kernel*> *m_pq;
    uint32_t m_bram_count;

};

#endif
