#ifndef RANDOM_HH_
#define RANDOM_HH_

#include <math.h>
#include <stdint.h>

class Random
{
public:

    Random(const int seed) : m_jsr(123456789 ^ seed)
    {

        const double m1 = 2147483648.0;
        const double vn = 9.91256303526217e-3;
        double dn = 3.442619855899;
        double tn = dn;
        const double q = vn / exp(-0.5 * dn * dn);

        m_kn[0] = (dn / q) * m1;
        m_kn[1] = 0;
        m_wn[0] = q / m1;
        m_wn[127] = dn / m1;
        m_fn[0] = 1.0;
        m_fn[127] = exp(-0.5 * dn * dn);
        for(uint32_t i = 126; i >= 1; i--) {
            dn = sqrt(-2.0 * log(vn / dn + exp(-0.5 * dn * dn)));
            m_kn[i + 1] = (dn / tn) * m1;
            tn = dn;
            m_fn[i] = exp(-0.5 * dn * dn);
            m_wn[i] = dn / m1;
        }

    }

    float Draw(const float mean, const float std)
    {
        return mean + rnor() * std;
    }

private:

    float rnor()
    {
        for(;;) {
            const int32_t hz = shr3();
            const uint32_t iz = hz & 127;
            if((uint32_t)abs(hz) < m_kn[iz]) {
                return hz * m_wn[iz];
            }
            float x = hz * m_wn[iz];
            if(iz == 0) {
                const float r = 3.442620f;
                float y;
                do {
                    x = -log(uni()) * 0.2904764;
                    y = -log(uni());
                } while(y + y < x * x);
                return (hz > 0) ? (r + x) : (-r - x);
            }
            if(m_fn[iz] + uni() * (m_fn[iz - 1] - m_fn[iz])
                    < exp(-0.5 * x * x)) {
                return x;
            }
        }
    }

    uint32_t shr3()
    {
        const uint32_t jz = m_jsr;
        m_jsr ^= m_jsr << 13;
        m_jsr ^= m_jsr >> 17;
        m_jsr ^= m_jsr << 5;
        return jz + m_jsr;
    }

    float uni()
    {
        return 0.5 + (int32_t)shr3() * 0.2328306e-9;
    }

    template<typename T>
    T abs(const T x) const
    {
        return x >= 0 ? x : -x;
    }

    uint32_t m_jsr;
    uint32_t m_kn[128];
    float m_wn[128];
    float m_fn[128];

};

#endif
