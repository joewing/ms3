#ifndef SIMULATOR_HH_
#define SIMULATOR_HH_

#include "QueueNetwork.hh"
#include "Kernel.hh"
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

    ~Simulator()
    {
        for(size_t i = 0; i < m_kernels.size(); i++) {
            delete m_kernels[i];
        }
    }

    void SetBRAMCount(const uint32_t bram_count)
    {
        m_bram_count = bram_count;
    }

    void AddKernel(const std::vector<uint32_t> &data)
    {
        Kernel * const k = new Kernel(&m_network, data);
        m_kernels.push_back(k);
    }

    void AddQueue(const uint32_t id, const uint32_t word_size)
    {
        Queue * const q = new Queue(word_size);
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

            // Increase the size of the bottleneck queue.
            const uint32_t word_size = m_network.GetWordSize(bottleneck);
            uint32_t increment = BRAM_BYTES / word_size;
            const uint32_t old_depth = m_network.GetDepth(bottleneck);
            if(old_depth == 1) {
                m_network.SetDepth(bottleneck, increment);
            } else {
                m_network.SetDepth(bottleneck, old_depth + increment);
            }
            const uint64_t t = Simulate();
            const int64_t delta = int64_t(best_value) - int64_t(t);
            if(delta <= 0) {
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
        Kernel * const k = reinterpret_cast<Kernel*>(arg);
        m_pq->Push(m_t, k);
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
        m_t = 0;
        while(likely(!m_pq->IsEmpty())) {
            Kernel * const k = m_pq->GetValue();
            m_t = m_pq->GetKey();
            m_pq->Pop();
            const uint64_t next_t = k->Process(m_t);
            if(next_t != 0) {
                m_pq->Push(next_t, k);
            } else if(k->IsBlocked()) {
                const uint32_t channel = k->GetBlockingChannel();
                m_network.RegisterNotification(channel, this, k);
            }
        }
        delete m_pq;
        return m_t;
    }

    QueueNetwork m_network;
    std::vector<Kernel*> m_kernels;
    PriorityQueue<uint64_t, Kernel*> *m_pq;
    uint64_t m_t;
    uint32_t m_bram_count;

};

#endif
