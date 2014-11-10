#ifndef TRACE_KERNEL_HH_
#define TRACE_KERNEL_HH_

#include "Kernel.hh"
#include "Decompress.hh"
#include <vector>

struct Access
{
    uint32_t delay;
    uint8_t channel;
    bool is_produce;
};

class TraceKernel : public Kernel
{
public:

    TraceKernel(const QueueNetwork * const network,
                const std::vector<uint32_t> &data) :
        Kernel(network),
        m_data(Decompress(data)),
        m_is_pending(false)
    {
    }

    virtual void Reset()
    {
        m_is_pending = false;
        m_data.Reset();
    }

    virtual uint64_t Process(uint64_t t)
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

    virtual bool IsBlocked() const
    {
        return m_is_pending;
    }

    virtual uint32_t GetBlockingChannel(const uint32_t index) const
    {
        if(index == 0) {
            return m_pending_access.channel;
        } else {
            return 0;
        }
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

    Decompress m_data;
    Access m_pending_access;
    bool m_is_pending;

};

#endif
