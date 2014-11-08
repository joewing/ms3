#ifndef KERNEL_HH_
#define KERNEL_HH_

#include "QueueNetwork.hh"
#include "Decompress.hh"
#include "qsim.h"

#include <cstdint>
#include <vector>

struct Access
{
    uint32_t delay;
    uint8_t channel;
    bool is_produce;
};

class Kernel
{
public:

    Kernel(const QueueNetwork * const network,
           const std::vector<uint32_t> &data) :
        m_network(network),
        m_data(Decompress(data)),
        m_is_pending(false)
    {
    }

    void Reset()
    {
        m_is_pending = false;
        m_data.Reset();
    }

    /** Process the next event. */
    uint64_t Process(uint64_t t)
    {
        if(!m_is_pending) {
            GetNext();
        }
        t += m_pending_access.delay;
        if(m_pending_access.is_produce) {
            if(m_network->Push(m_pending_access.channel, t)) {
                m_is_pending = false;
                return t;
            }
        } else {
            if(m_network->Pop(m_pending_access.channel, t)) {
                m_is_pending = false;
                return t;
            }
        }
        return 0;
    }

    bool IsBlocked() const
    {
        return m_is_pending;
    }

    uint32_t GetBlockingChannel() const
    {
        return m_pending_access.channel;
    }

private:

    bool HasNext() const
    {
        return m_data.HasNext();
    }

    void GetNext()
    {
        const uint32_t value = m_data.GetNext();
        m_pending_access.is_produce = (value >> 31) & 1;
        m_pending_access.channel = (value >> 24) & 0x7F;
        m_pending_access.delay = value & 0x00FFFFFF;
        m_is_pending = true;
    }

    const QueueNetwork * const m_network;
    Decompress m_data;
    Access m_pending_access;
    bool m_is_pending;

};

#endif
