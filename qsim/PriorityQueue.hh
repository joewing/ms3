#ifndef PRIORITY_QUEUE_HH_
#define PRIORITY_QUEUE_HH_

#include "qsim.h"

#include <cstdint>
#include <utility>
#include <cassert>

template<typename K, typename V>
class PriorityQueue
{
private:

    struct Node
    {
        K key;
        V value;
    };

public:

    PriorityQueue(const uint32_t max_depth) :
        m_max_depth(max_depth),
        m_size(0)
    {
        m_data = new Node[max_depth + 1];
    }

    ~PriorityQueue()
    {
        delete [] m_data;
    }

    bool IsEmpty() const
    {
        return m_size == 0;
    }

    void Push(const K key, const V value)
    {
        assert(m_size < m_max_depth);
        m_size += 1;
        m_data[m_size].key = key;
        m_data[m_size].value = value;
        uint32_t i = m_size;
        while(i != 1) {
            const uint32_t ni = i / 2;
            if(m_data[i].key < m_data[ni].key) {
                i = Swap(i, ni);
            } else {
                break;
            }
        }
    }

    void Pop()
    {
        m_data[1] = m_data[m_size];
        m_size -= 1;
        uint32_t i = 1;
        for(;;) {
            const uint32_t left = i * 2;
            const uint32_t right = left + 1;
            if(right <= m_size) {
                if(m_data[left].key < m_data[right].key) {
                    if(m_data[i].key > m_data[left].key) {
                        i = Swap(i, left);
                    } else {
                        break;
                    }
                } else if(m_data[i].key > m_data[right].key) {
                    i = Swap(i, right);
                } else {
                    break;
                }
            } else if(left <= m_size && m_data[i].key > m_data[left].key) {
                i = Swap(i, left);
            } else {
                break;
            }
        }
    }

    K GetKey() const
    {
        return m_data[1].key;
    }

    V GetValue() const
    {
        return m_data[1].value;
    }

private:

    uint32_t Swap(const uint32_t a, const uint32_t b)
    {
        const Node temp = m_data[a];
        m_data[a] = m_data[b];
        m_data[b] = temp;
        return b;
    }

    const uint32_t m_max_depth;
    uint32_t m_size;
    Node *m_data;

};

#endif
