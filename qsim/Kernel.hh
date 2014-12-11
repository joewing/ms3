#ifndef KERNEL_HH_
#define KERNEL_HH_

#include "QueueNetwork.hh"
#include "qsim.h"

#include <cstdint>

class EndSimulation
{
public:

    EndSimulation(const uint64_t t) : m_time(t)
    {
    }

    uint64_t GetTime() const
    {
        return m_time;
    }

private:

    const uint64_t m_time;

};

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

    virtual int64_t Process(uint64_t t) = 0;

    virtual uint32_t GetBlockingChannel(uint32_t index) const = 0;

    bool IsLast() const
    {
        return m_last;
    }

protected:

    const QueueNetwork * const m_network;
    const bool m_last;

};

#endif
