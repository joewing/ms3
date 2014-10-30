#ifndef PRIORITY_QUEUE_HH_
#define PRIORITY_QUEUE_HH_

#include "qsim.h"

#include <cstddef>
#include <utility>

template<typename K, typename V>
class PriorityQueue
{
public:

    PriorityQueue(const size_t max_depth) : m_size(0)
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
        m_data[m_size].first = key;
        m_data[m_size].second = value;
        size_t i = m_size;
        while(i != 1) {
            const size_t ni = i / 2;
            if(m_data[i].first < m_data[ni].first) {
                Swap(i, ni);
                i = ni;
            } else {
                break;
            }
        }
    }

    void Pop()
    {
        m_data[1] = m_data[m_size];
        m_size -= 1;
        size_t i = 1;
        for(;;) {
            const size_t left = i * 2;
            const size_t right = left + 1;
            if(right <= m_size) {
                if(m_data[left].first < m_data[right].first) {
                    if(m_data[i].first > m_data[left].first) {
                        Swap(i, left);
                        i = left;
                    } else {
                        break;
                    }
                } else if(m_data[i].first > m_data[right].first) {
                    Swap(i, right);
                    i = right;
                } else {
                    break;
                }
            } else if(left <= m_size && m_data[i].first > m_data[left].first) {
                Swap(i, left);
                i = left;
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

    void Swap(const size_t a, const size_t b)
    {
        const std::pair<K, V> temp = m_data[a];
        m_data[a] = m_data[b];
        m_data[b] = temp;
    }

    size_t m_size;
    std::pair<K, V> *m_data;

};

#endif
