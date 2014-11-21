#ifndef TRACE_KERNEL_HH_
#define TRACE_KERNEL_HH_

#include "Kernel.hh"
#include "Decompress.hh"
#include <vector>

class TraceKernel : public Kernel
{
public:

    TraceKernel(const QueueNetwork * const network,
                const bool last,
                const std::vector<uint32_t> &data) :
        Kernel(network, last),
        m_data(Decompress(data)),
        m_next(0),
        m_pending_channel(0),
        m_pending_is_produce(false)
    {
    }

    virtual void Reset() override
    {
        m_next = 0;
        m_pending_channel = 0;
        m_pending_is_produce = false;
        m_data.Reset();
        GetNext(1);
    }

    virtual uint64_t Process(const uint64_t t) override
    {

        // Return the amount of time left to wait, if any.
        if(m_next > t) {
            return m_next;
        }

        // Process the next push/pop.
        if(m_pending_is_produce) {
            if(m_network->Push(m_pending_channel, t)) {
                GetNext(t);
                return t;
            }
        } else {
            if(m_network->Pop(m_pending_channel, t)) {
                GetNext(t);
                return t;
            }
        }
        return 0;
    }

    virtual uint32_t GetBlockingChannel(const uint32_t index) const override
    {
        if(index == 0) {
            return m_pending_channel;
        } else {
            return 0;
        }
    }

private:

    void GetNext(const uint64_t t)
    {
        if(!m_data.HasNext()) {
            if(m_last) {
                throw EndSimulation(t);
            }
            m_data.Reset();
        }
        m_next = t;
        do {
            assert(m_data.HasNext());
            const uint32_t value = m_data.GetNext();
            m_pending_is_produce = (value >> 31) & 1;
            m_pending_channel = (value >> 24) & 0x7F;
            m_next += value & 0x00FFFFFF;
        } while(m_pending_channel == 0);
    }

    Decompress m_data;
    uint64_t m_next;
    uint8_t m_pending_channel;
    bool m_pending_is_produce;

};

#endif
