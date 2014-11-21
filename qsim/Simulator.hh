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
    }

    uint64_t Run()
    {
        uint32_t bram_count = m_bram_count;

        // Start all queues with a depth of 1.
        m_network.ResetDepths();

        // Increase the size of the bottleneck queue until
        // performance no longer improves.
        uint64_t best_value = Simulate();
        while(bram_count > 0) {

            // Find the bottleneck.
            const size_t bottleneck = m_network.GetBottleneck();
            assert(bottleneck != 0);

            // Increase the size of the bottleneck queue.
            const uint32_t word_size = m_network.GetWordSize(bottleneck);
            const uint32_t increment = std::max(uint32_t(1),
                                                BRAM_BYTES / word_size);
            const uint32_t old_depth = m_network.GetDepth(bottleneck);
            if(old_depth == 1) {
                m_network.SetDepth(bottleneck, increment);
            } else {
                m_network.SetDepth(bottleneck, old_depth + increment);
            }
            const uint64_t t = Simulate();
            if(best_value <= t) {
                // No improvement.
                m_network.SetDepth(bottleneck, old_depth);
                break;
            }
            best_value = t;
            bram_count -= 1;

        }

        return best_value;

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
            while(!m_pq->IsEmpty()) {
                Kernel * const k = m_pq->GetValue();
                t = std::max(t, m_pq->GetKey());
                m_pq->Pop();
                const uint64_t next_t = k->Process(t);
                if(next_t != 0) {
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
    PriorityQueue<uint64_t, Kernel*> *m_pq;
    uint32_t m_bram_count;

};

#endif
