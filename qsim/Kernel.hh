#ifndef KERNEL_HH_
#define KERNEL_HH_

#include "QueueNetwork.hh"
#include "qsim.h"

#include <cstdint>

class Kernel
{
public:

    Kernel(const QueueNetwork * const network, const bool last) :
        m_network(network),
        m_last(last)
    {
    }

    virtual ~Kernel()
    {
    }

    virtual void Reset() = 0;

    virtual uint64_t Process(uint64_t t) = 0;

    virtual bool IsBlocked() const = 0;

    virtual uint32_t GetBlockingChannel(uint32_t index) const = 0;

protected:

    const QueueNetwork * const m_network;
    const bool m_last;

};

#endif
