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
            m_depths.resize(id + 1);
        }
        m_queues[id] = queue;
        m_depths[id] = 1;
    }

    void RegisterNotification(const uint32_t id,
                              Observer * const obs,
                              void * const arg)
    {
        m_queues[id]->RegisterNotification(obs, arg);
    }

    bool Push(const uint32_t id, const uint64_t t) const
    {
        return m_queues[id]->Push(t);
    }

    bool Pop(const uint32_t id, const uint64_t t) const
    {
        return m_queues[id]->Pop(t);
    }

    uint32_t GetWordSize(const uint32_t id) const
    {
        return m_queues[id]->GetWordSize();
    }

    uint32_t GetDepth(const uint32_t id) const
    {
        return m_depths[id];
    }

    void SetDepth(const uint32_t id, const uint32_t depth)
    {
        m_depths[id] = depth;
    }

    void ResetDepths()
    {
        for(size_t i = 0; i < m_depths.size(); i++) {
            m_depths[i] = 1;
        }
    }

    void Reset()
    {
        for(size_t i = 0; i < m_queues.size(); i++) {
            Queue * const q = m_queues[i];
            if(q != nullptr) {
                q->Reset(m_depths[i]);
            }
        }
    }

    uint32_t GetBottleneck() const
    {
        uint32_t bottleneck = 0;
        uint64_t blocked = 0;
        for(uint32_t i = 0; i < m_queues.size(); i++) {
            const Queue * const q = m_queues[i];
            if(q != nullptr) {
                if(q->GetBlockedTime() > blocked) {
                    blocked = q->GetBlockedTime();
                    bottleneck = i;
                }
            }
        }
        return bottleneck;
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
    std::vector<uint32_t> m_depths;

};

#endif
