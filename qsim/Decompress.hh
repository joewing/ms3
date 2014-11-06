
#ifndef DECOMPRESS_HH_
#define DECOMPRESS_HH_

#include <vector>
#include <cstdint>

#define DICT_SIZE 65536

class Decompress
{
public:

    Decompress(const std::vector<uint32_t> &data) : m_data(data)
    {
        m_dictionary = new uint32_t[DICT_SIZE];
        m_index = m_data[0] >> 16;
        m_count = m_data[0] & 0xFFFF;
        m_value = m_data[1];
        m_position = m_data.size() > 2 ? 2 : 0;
    }

    ~Decompress()
    {
        delete [] m_dictionary;
    }

    uint32_t GetNext()
    {
        if (m_count > 0)
        {
            const uint32_t result = m_dictionary[m_index];
            m_count -= 1;
            m_index = (m_index + 1) % DICT_SIZE;
            return result;
        }
        const uint32_t result = m_value;
        m_dictionary[m_index] = m_value;
        m_index = m_data[m_position] >> 16;
        m_count = m_data[m_position] & 0xFFFF;
        m_value = m_data[m_position + 1];
        m_position += 2;
        if(m_position >= m_data.size())
        {
            m_position = 0;
        }
        return result;
    }

private:

    const std::vector<uint32_t> m_data;
    uint32_t *m_dictionary;
    uint32_t m_position;        // Position in data stream.
    uint32_t m_count;           // Number of outputs remaining.
    uint32_t m_index;           // Index into the dictionary.
    uint32_t m_value;           // Next value to add to the dictionary.

};

#endif
