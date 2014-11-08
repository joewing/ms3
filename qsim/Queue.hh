#ifndef QUEUE_HH_
#define QUEUE_HH_

#include "qsim.h"

#include <cstdint>
#include <algorithm>
#include <vector>

class Queue
{
public:

    Queue(const uint32_t word_size) :
        m_word_size(word_size),
        m_observer(nullptr),
        m_observer_arg(nullptr)
    {
        Reset(1);
    }

    void Reset(const uint32_t depth)
    {
        m_max_depth = depth;
        m_depth = 0;
        m_blocked = 0;
        m_blocked_start = 0;
    }

    bool Push(const uint64_t t)
    {
        if(m_blocked_start != 0) {
            m_blocked += t - m_blocked_start;
            m_blocked_start = 0;
        }
        if(m_depth < m_max_depth) {
            m_depth += 1;
            if(m_observer != nullptr) {
                m_observer->Notify(m_observer_arg);
                m_observer = nullptr;
            }
            return true;
        } else {
            m_blocked_start = t;
            return false;
        }
    }

    bool Pop(const uint64_t t)
    {
        if(m_blocked_start != 0) {
            m_blocked += t - m_blocked_start;
            m_blocked_start = 0;
        }
        if(m_depth > 0) {
            m_depth -= 1;
            if(m_observer != nullptr) {
                m_observer->Notify(m_observer_arg);
                m_observer = nullptr;
            }
            return true;
        } else {
            m_blocked_start = t;
            return false;
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

    uint32_t GetDepth() const
    {
        return m_max_depth;
    }

    void RegisterNotification(Observer * const obs,
                              void * const arg)
    {
        m_observer = obs;
        m_observer_arg = arg;
    }

private:

    const uint32_t m_word_size;
    Observer *m_observer;
    void *m_observer_arg;
    uint64_t m_blocked;
    uint64_t m_blocked_start;
    uint32_t m_max_depth;
    uint32_t m_depth;

};

#endif