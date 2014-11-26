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
        m_max_depth(1),
        m_push_time(0),
        m_pop_time(0),
        m_block_start(0),
        m_push_blocked(false),
        m_pop_blocked(false)
    {
        Reset();
    }

    void Reset()
    {
        m_depth = 0;
        m_observer = nullptr;
        m_pop_blocked = false;
        m_push_blocked = false;
        m_block_start = 0;
        m_push_time = 0;
        m_pop_time = 0;
    }

    bool Push(const uint64_t t)
    {
        if(m_depth < m_max_depth) {
            m_depth += 1;
            if(m_pop_blocked) {
                m_pop_time += t - m_block_start;
                m_pop_blocked = false;
            }
            if(m_observer != nullptr) {
                m_observer->Notify(m_observer_arg);
                m_observer = nullptr;
            }
            return true;
        } else {
            assert(!m_pop_blocked);
            if(!m_push_blocked) {
                m_block_start = t;
                m_push_blocked = true;
            }
            return false;
        }
    }

    bool Pop(const uint64_t t)
    {
        if(m_depth > 0) {
            m_depth -= 1;
            if(m_push_blocked) {
                m_push_time += t - m_block_start;
                m_push_blocked = false;
            }
            if(m_observer != nullptr) {
                m_observer->Notify(m_observer_arg);
                m_observer = nullptr;
            }
            return true;
        } else {
            assert(!m_push_blocked);
            if(!m_pop_blocked) {
                m_block_start = t;
                m_pop_blocked = true;
            }
            return false;
        }
    }

    uint64_t GetBlockedTime() const
    {
        return std::min(m_push_time, m_pop_time);
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
        m_observer = obs;
        m_observer_arg = arg;
    }

private:

    const uint32_t m_id;
    const uint32_t m_word_size;
    Observer *m_observer;
    void *m_observer_arg;
    uint32_t m_max_depth;
    uint32_t m_depth;
    uint64_t m_push_time;
    uint64_t m_pop_time;
    uint64_t m_block_start;
    bool m_push_blocked;
    bool m_pop_blocked;

};

#endif
