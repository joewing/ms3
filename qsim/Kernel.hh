#ifndef KERNEL_HH_
#define KERNEL_HH_

#include "QueueNetwork.hh"
#include "qsim.h"

#include <cstdint>

class Kernel
{
public:

    Kernel(const QueueNetwork * const network) :
        m_network(network)
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

};

#endif
