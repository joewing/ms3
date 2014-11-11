#ifndef QUEUE_HH_
#define QUEUE_HH_

#include "qsim.h"

#include <cassert>
#include <cstdint>
#include <algorithm>
#include <vector>

class Queue
{
public:

    Queue(const uint32_t id, const uint32_t word_size) :
        m_id(id),
        m_word_size(word_size),
        m_observer(nullptr),
        m_observer_arg(nullptr),
        m_max_depth(1)
    {
        Reset();
    }

    void Reset()
    {
        m_depth = 0;
        m_blocked_time = 0;
        m_blocked_start = 0;
        m_pop_blocked = false;
        m_push_blocked = false;
        m_observer = nullptr;
    }

    bool Push(const uint64_t t)
    {
        if(m_pop_blocked) {
            m_blocked_time += t - m_blocked_start;
            m_blocked_start = t;
        }
        if(m_depth < m_max_depth) {
            m_depth += 1;
            if(m_observer != nullptr) {
                m_observer->Notify(m_observer_arg);
                m_observer = nullptr;
            }
            m_pop_blocked = false;
            return true;
        } else {
            m_blocked_start = t;
            m_push_blocked = true;
            return false;
        }
    }

    bool Pop(const uint64_t t)
    {
        if(m_push_blocked) {
            m_blocked_time += t - m_blocked_start;
            m_blocked_start = t;
        }
        if(m_depth > 0) {
            m_depth -= 1;
            if(m_observer != nullptr) {
                m_observer->Notify(m_observer_arg);
                m_observer = nullptr;
            }
            m_push_blocked = false;
            return true;
        } else {
            m_blocked_start = t;
            m_pop_blocked = true;
            return false;
        }
    }

    uint64_t GetBlockedTime() const
    {
        return m_blocked_time;
    }

    uint32_t GetWordSize() const
    {
        return m_word_size;
    }

    uint32_t GetDepth() const
    {
        return m_max_depth;
    }

    void SetDepth(uint32_t depth)
    {
        m_max_depth = depth;
    }

    void RegisterNotification(Observer * const obs,
                              void * const arg)
    {
        assert(m_observer == nullptr || m_observer == obs);
        assert(m_push_blocked || m_pop_blocked);
        m_observer = obs;
        m_observer_arg = arg;
    }

private:

    const uint32_t m_id;
    const uint32_t m_word_size;
    Observer *m_observer;
    void *m_observer_arg;
    uint64_t m_blocked_time;
    uint64_t m_blocked_start;
    bool m_push_blocked;
    bool m_pop_blocked;
    uint32_t m_max_depth;
    uint32_t m_depth;

};

#endif
