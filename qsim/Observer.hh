#ifndef OBSERVER_HH_
#define OBSERVER_HH_

class Observer
{
public:
    virtual void Notify(void *arg) = 0;
};

#endif
