#ifndef SPLIT_KERNEL_HH_
#define SPLIT_KERNEL_HH_

#include "Kernel.hh"

class SplitKernel : public Kernel
{
public:

    SplitKernel(const QueueNetwork * const network,
                const uint32_t in_port,
                const uint32_t out0,
                const uint32_t out1) :
        Kernel(network, false),
        m_in(in_port),
        m_out0(out0),
        m_out1(out1),
        m_last(out1),
        m_pending(false),
        m_blocked(false)
    {
    }

    virtual void Reset()
    {
        m_last = m_out0;
        m_pending = false;
        m_blocked = false;
    }

    virtual uint64_t Process(uint64_t t)
    {

        // Read a value.
        if(!m_pending) {
            if(m_network->Pop(m_in, t)) {
                m_pending = true;
                m_blocked = false;
                return t + 1;
            } else {
                m_blocked = true;
                return 0;
            }
        }

        // Determine the next priority.
        uint32_t a, b;
        if(m_last == m_out0) {
            a = m_out0;
            b = m_out1;
            m_last = m_out1;
        } else {
            a = m_out1;
            b = m_out0;
            m_last = m_out0;
        }

        // Attempt to send the value.
        if(m_network->Push(a, t)) {
            m_pending = false;
            m_blocked = false;
            return t + 1;
        } else if(m_network->Push(b, t)) {
            m_pending = false;
            m_blocked = false;
            return t + 1;
        } else {
            m_blocked = true;
            return 0;
        }
    }

    virtual bool IsBlocked() const
    {
        // The virtual trace never ends.
        return m_blocked;
    }

    uint32_t GetBlockingChannel(uint32_t index) const
    {
        if(!m_pending && index == 0) {
            return m_in;
        } else if(m_pending) {
            if(index == 0) {
                return m_out0;
            } else if(index == 1) {
                return m_out1;
            }
        }
        return 0;
    }

private:

    const uint32_t m_in;
    const uint32_t m_out0;
    const uint32_t m_out1;
    uint32_t m_last;
    bool m_pending;
    bool m_blocked;

};

#endif
