#ifndef QUEUE_HH_
#define QUEUE_HH_

#include "Decompress.hh"
#include "qsim.h"

#include <cstdint>
#include <algorithm>
#include <vector>

class Queue
{
public:

    Queue(const uint32_t word_size,
          const std::vector<uint32_t> prod_data,
          const std::vector<uint32_t> cons_data) :
        m_prod(prod_data),
        m_cons(cons_data),
        m_word_size(word_size)
    {
        Reset(1);
    }

    uint64_t Reset(const uint32_t depth)
    {
        m_max_depth = depth;
        m_depth = 0;
        m_blocked = 0;
        m_blocked_start = 0;
        m_prod.Reset();
        m_cons.Reset();
        m_next_prod = m_prod.GetNext();
        m_next_cons = std::max(uint64_t(1), uint64_t(m_cons.GetNext()));
        return std::min(m_next_prod, m_next_cons);
    }

    uint64_t Process(const uint64_t t)
    {
        if(t >= m_next_prod) {
            if(m_depth < m_max_depth) {
                // Not blocked.
                m_depth += 1;
                if(m_prod.HasNext()) {
                    m_next_prod = t + m_prod.GetNext();
                } else {
                    m_next_prod = UINT64_MAX;
                }
            } else {
                // Blocked.
                m_blocked_start = std::max(t, m_blocked_start);
                m_next_prod = m_next_cons + 1;
            }
        }
        if(t >= m_next_cons) {
            if(m_depth != 0) {
                m_depth -= 1;
                m_total += 1;
                if(m_cons.HasNext()) {
                    m_next_cons = t + m_cons.GetNext();
                } else {
                    m_next_cons = 0;
                }
                if(m_blocked_start != 0) {
                    m_blocked += t - m_blocked_start;
                    m_blocked_start = 0;
                }
            } else {
                m_next_cons = m_next_prod + 1;
            }
        }
        return std::min(m_next_prod, m_next_cons);
    }

    uint64_t GetBlocked() const
    {
        return m_blocked;
    }

    uint32_t GetWordSize() const
    {
        return m_word_size;
    }

private:

    Decompress m_prod;
    Decompress m_cons;
    const uint32_t m_word_size;

    uint64_t m_total;
    uint64_t m_blocked;
    uint64_t m_blocked_start;
    uint32_t m_max_depth;
    uint32_t m_depth;

    uint64_t m_next_prod;
    uint64_t m_next_cons;

};

#endif
