#ifndef PRIORITY_QUEUE_HH_
#define PRIORITY_QUEUE_HH_

#include "qsim.h"

#include <cstdint>
#include <utility>

template<typename K, typename V>
class PriorityQueue
{
public:

    PriorityQueue(const uint32_t max_depth) : m_size(0)
    {
        m_data = new std::pair<K, V>[max_depth + 1];
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
        m_size += 1;
        m_data[m_size] = std::make_pair(key, value);
        uint32_t i = m_size;
        while(i != 1) {
            const uint32_t ni = i / 2;
            if(m_data[i].first < m_data[ni].first) {
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
                if(m_data[left].first < m_data[right].first) {
                    if(m_data[i].first > m_data[left].first) {
                        i = Swap(i, left);
                    } else {
                        break;
                    }
                } else if(m_data[i].first > m_data[right].first) {
                    i = Swap(i, right);
                } else {
                    break;
                }
            } else if(left <= m_size && m_data[i].first > m_data[left].first) {
                i = Swap(i, left);
            } else {
                break;
            }
        }
    }

    K GetKey() const
    {
        return m_data[1].first;
    }

    V GetValue() const
    {
        return m_data[1].second;
    }

private:

    uint32_t Swap(const uint32_t a, const uint32_t b)
    {
        const std::pair<K, V> temp = m_data[a];
        m_data[a] = m_data[b];
        m_data[b] = temp;
        return b;
    }

    uint32_t m_size;
    std::pair<K, V> *m_data;

};

#endif
