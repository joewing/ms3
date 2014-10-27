#ifndef RANDOM_H_
#define RANDOM_H_

#include <stdlib.h>
#include <stdint.h>
#include <math.h>

class Random {
public:

    Random(uint32_t seed) :
        m_index(0),
        m_spare(false),
        m_rand1(0.0),
        m_rand2(0.0)
    {
        for(int i = 0; i < 624; i++) {
            m_state[i] = seed;
            seed = 0x6c078965 * (seed ^ (seed >> 30));
            seed += i;
        }
    }

    uint32_t uniform_int(uint32_t min = 0, uint32_t max = 0xFFFFFFFF) {
        const uint32_t temp = next();
        if(max - min < 0xFFFFFFFF) {
            return min + temp % (max - min + 1);
        } else {
            return temp;
        }
    }

    double uniform_double(double min = -1.0, double max = 1.0) {
        const double temp = (double)next();
        return min + (max - min) * temp / (double)0xFFFFFFFFUL;
    }

    double normal_double(double mean = 0.0, double var = 1.0) {
        if(m_spare) {
            m_spare = false;
            return mean + sqrt(var * m_rand1) * sin(m_rand2);
        }
        m_spare = true;
        m_rand1 = uniform_double(0.0, 1.0);
        m_rand1 = (m_rand1 < 1e-100) ? 1e-100 : m_rand1;
        m_rand1 = -2.0 * log(m_rand1);
        m_rand2 = uniform_double(0.0, M_PI * 2.0);
        return mean + sqrt(var * m_rand1) * cos(m_rand2);
    }

private:

    uint32_t next() {
        if(m_index == 624) {
            for(uint32_t i = 0; i < 624; i++) {
                uint32_t j = (i + 1) % 624;
                uint32_t y = m_state[i] >> 31;
                y += m_state[j] & 0x7FFFFFFF;
                j = (i + 397) % 624;
                m_state[i] = m_state[j] ^ (y >> 1);
                if(y & 1) {
                    m_state[i] &= 0x9908B0DF;
                }
            }
            m_index = 0;
        }
        uint32_t y = m_state[m_index];
        y ^= (y >> 11);
        y ^= (y << 7) & 0x9D2C5680;
        y ^= (y << 15) & 0xEFC60000;
        y ^= (y >> 18);
        m_index += 1;
        return y;
    }

    uint32_t m_index;
    uint32_t m_state[624];
    bool m_spare;
    double m_rand1;
    double m_rand2;

};

#endif
