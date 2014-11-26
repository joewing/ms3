#ifndef QUEUE_NETWORK_HH_
#define QUEUE_NETWORK_HH_

#include "Observer.hh"
#include "Queue.hh"

#include <vector>

class QueueNetwork
{
public:

    QueueNetwork()
    {
    }

    ~QueueNetwork()
    {
        for(size_t i = 0; i < m_queues.size(); i++) {
            if(m_queues[i] != NULL) {
                delete m_queues[i];
            }
        }
    }

    void AddQueue(const uint32_t id, Queue * const queue)
    {
        if(m_queues.size() <= id) {
            m_queues.resize(id + 1);
        }
        m_queues[id] = queue;
    }

    void RegisterNotification(const uint32_t id,
                              Observer * const obs,
                              void * const arg)
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        m_queues[id]->RegisterNotification(obs, arg);
    }

    bool Push(const uint32_t id, const uint64_t t) const
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        return m_queues[id]->Push(t);
    }

    bool Pop(const uint32_t id, const uint64_t t) const
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        return m_queues[id]->Pop(t);
    }

    uint32_t GetWordSize(const uint32_t id) const
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        return m_queues[id]->GetWordSize();
    }

    uint32_t GetDepth(const uint32_t id) const
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        return m_queues[id]->GetDepth();
    }

    void SetDepth(const uint32_t id, const uint32_t depth)
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        m_queues[id]->SetDepth(depth);
    }

    uint32_t GetBlockedTime(const uint32_t id) const
    {
        assert(id < m_queues.size());
        assert(m_queues[id] != nullptr);
        return m_queues[id]->GetBlockedTime();
    }

    void Reset()
    {
        for(size_t i = 0; i < m_queues.size(); i++) {
            Queue * const q = m_queues[i];
            if(q != nullptr) {
                q->Reset();
            }
        }
    }

    std::vector<std::pair<uint32_t, uint32_t> > GetDepths() const
    {
        std::vector<std::pair<uint32_t, uint32_t> > result;
        for(uint32_t i = 0; i < m_queues.size(); i++) {
            const Queue * const q = m_queues[i];
            if(q != nullptr) {
                const uint32_t depth = q->GetDepth();
                const std::pair<uint32_t, uint32_t> p(i, depth);
                result.push_back(p);
            }
        }
        return result;
    }

private:

    std::vector<Queue*> m_queues;

};

#endif
