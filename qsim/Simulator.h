#ifndef SIMULATOR_H_
#define SIMULATOR_H_

#include <queue>

#include "Random.h"
#include "Queue.h"

#define BRAM_BYTES ((512 * 72) / 8)

class Simulator
{
public:

    Simulator(uint32_t seed, double epsilon = 1e-3) :
        m_random(seed),
        m_epsilon(epsilon)
    {
    }

    void AddQueue(uint32_t count, uint32_t word_size,
                  double ptime, double pvar,
                  double ctime, double cvar)
    {
        if(ptime > 0.0 && ctime > 0.0) {
            Queue *q = new Queue(&m_random, count, word_size,
                                 ptime, pvar, ctime, cvar);
            m_queues.push_back(q);
            m_depths.push_back(1);
        }
    }

    uint64_t Run(uint32_t bram_count)
    {

        // Start all queues with a depth of 1.
        uint32_t max_count = 0;
        for(size_t i = 0; i < m_queues.size(); i++) {
            m_depths[i] = 1;
            const uint32_t count = m_queues[i]->GetCount();
            max_count = count > max_count ? count : max_count;
        }

        // Scale the queue run counts.
        const uint32_t max_value = 1000;
        const uint32_t scale = max_count > max_value ?
                               max_count / max_value : 1;
        for(size_t i = 0; i < m_queues.size(); i++) {
            uint32_t count = m_queues[i]->GetCount();
            count = (count + scale - 1) / scale;
            m_queues[i]->SetCount(count);
        }

        // Increase the size of the bottleneck queue until
        // performance no longer improves.
        uint64_t best_value = SimulateMultiple();
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
            const uint64_t t = SimulateMultiple();
            const int64_t delta = (int64_t)t - (int64_t)best_value;
            if(delta <= 0) {
                // No improvement.
                m_depths[bottleneck] = old_depth;
                break;
            }
            bram_count -= 1;

        }

        return Round(best_value);

    }

    std::vector<uint32_t> GetDepths() const
    {
        return m_depths;
    }

private:

    struct Node
    {

        Node(Queue *q, uint64_t t) : m_q(q), m_t(t)
        {
        }

        Queue *m_q;
        uint64_t m_t;

        bool operator<(const Node &other) const
        {
            return m_t < other.m_t;
        }

    };

    uint64_t Simulate() {
        std::priority_queue<Node> pq;
        for(size_t i = 0; i < m_queues.size(); i++) {
            Queue *q = m_queues[i];
            const uint64_t t = q->Reset(m_depths[i]);
            pq.push(Node(q, t));
        }
        uint64_t t = 0;
        while(!pq.empty()) {
            Node n = pq.top();
            pq.pop();
            t = t > n.m_t ? t : n.m_t;
            const uint64_t next_t = n.m_q->Process(t);
            if(next_t > 0) {
                pq.push(Node(n.m_q, next_t));
            }
        }
        return t;
    }

    uint64_t Round(uint64_t t) const
    {
        const int64_t min_delta = (int64_t)(1.0 / m_epsilon);
        const uint32_t sfigs = (uint32_t)(ceil(log10((double)min_delta)));
        const uint32_t figs = (uint32_t)(ceil(log10((double)t)));
        if(figs > sfigs) {
            const uint64_t div = (uint64_t)round(pow(10.0, figs - sfigs));
            t /= div;
            t *= div;
        }
        return t;
    }

    uint64_t SimulateMultiple(double confidence = 0.95)
    {
        double n = 0.0;
        double mean = 0.0;
        double m2 = 0.0;
        for(;;) {
            const uint64_t t = Simulate();
            n += 1.0;
            const double delta = (double)t - mean;
            mean += delta / n;
            m2 += delta * ((double)t - mean);
            if(n > 2.0) {
                const double var = m2 / (n - 1.0);
                const double std = sqrt(var);
                const double interval = confidence * std / sqrt(n);
                if(interval / mean < m_epsilon) {
                    break;
                }
            }
        }
        return Round((uint64_t)mean);
    }

    Random m_random;
    const double m_epsilon;
    std::vector<Queue*> m_queues;
    std::vector<uint32_t> m_depths;

};

#endif
