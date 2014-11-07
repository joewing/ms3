#ifndef SIMULATOR_HH_
#define SIMULATOR_HH_

#include "Queue.hh"
#include "PriorityQueue.hh"
#include "qsim.h"

#include <algorithm>

#define BRAM_BYTES ((512 * 72) / 8)

class Simulator
{
public:

    Simulator()
    {
    }

    ~Simulator()
    {
        for(size_t i = 0; i < m_queues.size(); i++) {
            delete m_queues[i];
        }
    }

    void AddQueue(const uint32_t word_size,
                  const std::vector<uint32_t> pdata,
                  const std::vector<uint32_t> cdata)
    {
        if(!pdata.empty() && !cdata.empty()) {
            Queue *q = new Queue(word_size, pdata, cdata);
            m_queues.push_back(q);
            m_depths.push_back(1);
        }
    }

    uint64_t Run(uint32_t bram_count)
    {

        // Start all queues with a depth of 1.
        for(size_t i = 0; i < m_queues.size(); i++) {
            m_depths[i] = 1;
        }

        // Increase the size of the bottleneck queue until
        // performance no longer improves.
        uint64_t best_value = Simulate();
        while(bram_count > 0) {

            // Find the bottleneck.
            size_t bottleneck = 0;
            for(size_t i = 1; i < m_queues.size(); i++) {
                const uint64_t bt = m_queues[bottleneck]->GetBlocked();
                const uint64_t t = m_queues[i]->GetBlocked();
                if(t > bt) {
                    bottleneck = i;
                }
            }

            // Increase the size of the bottleneck queue.
            const uint32_t word_size = m_queues[bottleneck]->GetWordSize();
            uint32_t increment = BRAM_BYTES / word_size;
            const uint32_t old_depth = m_depths[bottleneck];
            if(old_depth == 1) {
                m_depths[bottleneck] = increment;
            } else {
                m_depths[bottleneck] += increment;
            }
            const uint64_t t = Simulate();
            const int64_t delta = int64_t(best_value) - int64_t(t);
            if(delta <= 0) {
                // No improvement.
                m_depths[bottleneck] = old_depth;
                break;
            }
            best_value = t;
            bram_count -= 1;

        }

        return best_value;

    }

    std::vector<uint32_t> GetDepths() const
    {
        return m_depths;
    }

private:

    uint64_t Simulate() const
    {
        const size_t queue_count = m_queues.size();
        PriorityQueue<uint64_t, Queue*> pq(queue_count);
        for(size_t i = 0; i < queue_count; i++) {
            Queue * const q = m_queues[i];
            const uint64_t t = q->Reset(m_depths[i]);
            pq.Push(t, q);
        }
        uint64_t t = 0;
        while(likely(!pq.IsEmpty())) {
            Queue * const q = pq.GetValue();
            t = pq.GetKey();
            pq.Pop();
            const uint64_t next_t = q->Process(t);
            if(likely(next_t != 0)) {
                pq.Push(next_t, q);
            }
        }
        return t;
    }

    std::vector<Queue*> m_queues;
    std::vector<uint32_t> m_depths;

};

#endif
