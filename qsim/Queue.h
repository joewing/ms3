#ifndef QUEUE_H_
#define QUEUE_H_

#include "Random.h"
#include <stdint.h>

class Queue {
public:

    Queue(Random *r,
          uint32_t count, uint32_t word_size,
          double ptime, double pvar,
          double ctime, double cvar) :
        m_random(r),
        m_count(count),
        m_word_size(word_size),
        m_pmean(ptime / (double)count),
        m_pvar(pvar),
        m_cmean(ctime / (double)count),
        m_cvar(cvar)
    {
        Reset(1);
    }

    uint64_t Reset(uint64_t depth)
    {
        m_depth = depth;
        m_size = 0;
        m_total = 0;
        m_blocked_start = 0;
        m_next_prod = GetNextProd(0);
        m_next_cons = GetNextCons(0);
        if(m_next_prod < m_next_cons) {
            return m_next_prod;
        } else {
            return m_next_cons;
        }
    }

    uint64_t Process(uint64_t t)
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
        if(m_total >= m_count) {
            return 0;
        } else if(m_next_prod < m_next_cons) {
            return m_next_prod;
        } else {
            return m_next_cons;
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

    void SetCount(uint32_t count)
    {
        m_count = count;
    }

private:

    uint64_t GetRand(double mean, double var) const
    {
        const double temp = m_random->normal_double(mean, var);
        if(temp <= 0.0) {
            return 1;
        }
        const uint64_t result = (uint64_t)(temp + 0.5);
        return result > 1 ? result : 1;
    }

    uint64_t GetNextProd(uint64_t t) const
    {
        return t + GetRand(m_pmean, m_pvar);
    }

    uint64_t GetNextCons(uint64_t t) const
    {
        return t + GetRand(m_cmean, m_cvar);
    }

    Random *m_random;
    uint64_t m_total;
    uint64_t m_blocked;
    uint64_t m_blocked_start;
    uint32_t m_count;
    const uint32_t m_word_size;
    uint32_t m_depth;
    uint32_t m_size;
    const double m_pmean;
    const double m_pvar;
    const double m_cmean;
    const double m_cvar;

    uint64_t m_next_prod;
    uint64_t m_next_cons;

};

#endif
