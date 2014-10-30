#ifndef QUEUE_HH_
#define QUEUE_HH_

#include "Random.hh"
#include "qsim.h"

#include <stdint.h>
#include <algorithm>

class Queue {
public:

    Queue(Random *const r,
          const uint32_t count, const uint32_t word_size,
          const float ptime, const float pvar,
          const float ctime, const float cvar) :
        m_random(r),
        m_count(count),
        m_word_size(word_size),
        m_pmean(ptime / (float)count),
        m_pstd(sqrt(pvar)),
        m_cmean(ctime / (float)count),
        m_cstd(sqrt(cvar))
    {
        Reset(1);
    }

    uint64_t Reset(const uint32_t depth)
    {
        m_depth = depth;
        m_size = 0;
        m_total = 0;
        m_blocked_start = 0;
        m_next_prod = GetNextProd(0);
        m_next_cons = GetNextCons(0);
        return std::min(m_next_prod, m_next_cons);
    }

    uint64_t Process(const uint64_t t)
    {
        if(m_blocked_start > 0) {
            // Currently blocked, update the amount of time we
            // have been blocked.
            m_blocked += t - m_blocked_start;
            m_blocked_start = t;
        }
        if(t >= m_next_prod) {
            if(m_size < m_depth) {
                // Not blocked.
                m_size += 1;
                m_next_prod = GetNextProd(t);
                m_blocked_start = 0;
            } else {
                // Blocked.
                m_blocked_start = t;
                m_next_prod = m_next_cons;
            }
        }
        if(t >= m_next_cons) {
            if(m_size > 0) {
                m_size -= 1;
                m_total += 1;
                m_next_cons = GetNextCons(t);
            } else {
                m_next_cons = m_next_prod;
            }
        }
        if(unlikely(m_total >= m_count)) {
            return 0;
        } else {
            return std::min(m_next_prod, m_next_cons);
        }
    }

    uint64_t GetBlocked() const
    {
        return m_blocked;
    }

    uint32_t GetWordSize() const
    {
        return m_word_size;
    }

    uint32_t GetCount() const
    {
        return m_count;
    }

    void SetCount(const uint32_t count)
    {
        m_count = count;
    }

private:

    uint64_t GetRand(const float mean, const float std) const
    {
        const float temp = m_random->Draw(mean, std);
        if(temp <= 1.0) {
            return 1;
        }
        return (uint64_t)(temp + 0.5);
    }

    uint64_t GetNextProd(const uint64_t t) const
    {
        return t + GetRand(m_pmean, m_pstd);
    }

    uint64_t GetNextCons(const uint64_t t) const
    {
        return t + GetRand(m_cmean, m_cstd);
    }

    Random * const m_random;
    uint64_t m_total;
    uint64_t m_blocked;
    uint64_t m_blocked_start;
    uint32_t m_count;
    const uint32_t m_word_size;
    uint32_t m_depth;
    uint32_t m_size;
    const float m_pmean;
    const float m_pstd;
    const float m_cmean;
    const float m_cstd;

    uint64_t m_next_prod;
    uint64_t m_next_cons;

};

#endif
