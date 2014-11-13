
#ifndef DECOMPRESS_HH_
#define DECOMPRESS_HH_

#include <vector>
#include <cstdint>
#include <cassert>

#define DICT_SIZE 65536

class Decompress
{
public:

    Decompress(const std::vector<uint32_t> &data) : m_data(data)
    {
        assert(data.size() > 0);
        assert((data.size() & 1) == 0);
        m_dictionary = new uint32_t[DICT_SIZE];
        for(size_t i = 0; i < DICT_SIZE; i++) {
            m_dictionary[i] = UINT32_MAX;
        }
        Reset();
    }

    ~Decompress()
    {
        delete [] m_dictionary;
    }

    bool HasNext() const
    {
        return !m_eod;
    }

    uint32_t GetNext()
    {
        assert(!m_eod);
        if (m_count > 0)
        {
            const uint32_t result = m_dictionary[m_index];
            m_count -= 1;
            m_index = (m_index + 1) % DICT_SIZE;
            m_eod = m_count == 0 && m_position >= m_data.size();
            return result;
        }
        const uint32_t result = m_value;
        m_dictionary[m_index] = m_value;
        if(m_position + 1 < m_data.size()) {
            m_index = m_data[m_position] >> 16;
            m_count = m_data[m_position] & 0xFFFF;
            m_value = m_data[m_position + 1];
            m_position += 2;
        } else {
            m_eod = true;
        }
        return result;
    }

    void Reset()
    {
        m_position = 0;
        m_index = m_data[0] >> 16;
        m_count = m_data[0] & 0xFFFF;
        m_value = m_data[1];
        m_position = 2;
        m_eod = false;
    }

private:

    const std::vector<uint32_t> m_data;
    uint32_t *m_dictionary;
    uint32_t m_position;        // Position in data stream.
    uint32_t m_count;           // Number of outputs remaining.
    uint32_t m_index;           // Index into the dictionary.
    uint32_t m_value;           // Next value to add to the dictionary.
    bool m_eod;

};

#endif
