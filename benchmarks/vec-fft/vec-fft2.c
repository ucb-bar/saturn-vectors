// See LICENSE for license details.

//**************************************************************************
// Vectorized decimation-in-frequency radix-2 FFT
//--------------------------------------------------------------------------
#include "fft2.h"

void fft2(float Xr[], float Xi[], const float Wr[], const float Wi[], size_t N, size_t M)
{
  {
    size_t N1, N2;
    float *end;

    end = Xr + N;

    for (N1 = N; N1 > 4;) {
      float *Xr0;
      float *Xi0;
      float *Xr1;
      float *Xi1;
      const float *wr;
      const float *wi;

      N2 = N1 / 2;

      Xr0 = Xr; // Lower half
      Xi0 = Xi;
      Xr1 = Xr + N2; // Upper half
      Xi1 = Xi + N2;

      // Iterate over butterfly groups
      do {
        size_t n;

        n = N2;
        wr = Wr;
        wi = Wi;
        // Iterate over butterflies in group
        do {
          size_t vl;
          __asm__ __volatile__ (
            "vsetvli %0, %1, e32, m4, ta, ma" "\n\t"
            : "=r" (vl)
            : "r" (n));

          __asm__ __volatile__ (
            "vle32.v v8,  %0" "\n\t" // ar
            "vle32.v v12, %1" "\n\t" // br
            "vle32.v v16, %2" "\n\t" // ai
            "vle32.v v20, %3" "\n\t" // bi

            "vfsub.vv  v24, v8,  v12" "\n\t"  // ar - br
            "vle32.v   v4,  %5"       "\n\t"  // Wi
            "vfsub.vv  v28, v16, v20" "\n\t"  // ai - bi
            "vle32.v   v0,  %4"       "\n\t"  // Wr
            "vfadd.vv  v16, v16, v20" "\n\t"  // ai' = ai + bi
            "vfmul.vv  v20, v24, v4"  "\n\t"  // Wi * (ar - br)
            "vfmul.vv  v4,  v28, v4"  "\n\t"  // Wi * (ai - bi)
            "vfadd.vv  v8,  v8,  v12" "\n\t"  // ar' = ar + br

            "vse32.v v16, %2" "\n\t" // ai'

            "vfmadd.vv v28, v0,  v20" "\n\t"  // bi' = Wr * (ai - bi) + Wi * (ar - br)
            "vfmsub.vv v24, v0,  v4"  "\n\t"  // br' = Wr * (ar - br) - Wi * (ai - bi)

            "vse32.v v8,  %0" "\n\t" // ar'
            "vse32.v v28, %3" "\n\t" // bi'
            "vse32.v v24, %1" "\n\t" // br'

            :
            : "A" (*Xr0), "A" (*Xr1), "A" (*Xi0), "A" (*Xi1), "A" (*wr), "A" (*wi));

          n -= vl;
          wr += vl;
          wi += vl;
          Xr0 += vl;
          Xi0 += vl;
          Xr1 += vl;
          Xi1 += vl;

        } while (n > 0);

        Xr0 = Xr1;
        Xi0 = Xi1;
        Xr1 += N2;
        Xi1 += N2;

      } while (Xr1 < end);

      Wr = wr;
      Wi = wi;
      N1 = N2;
    }
  }

  {
    float *xr;
    float *xi;
    size_t n;

    /* Stage M-2 */
    xr = Xr;
    xi = Xi;
    n = N / 4;
    do {
      size_t vl;
      __asm__ __volatile__ (
        "vsetvli %0, %1, e32, m2, ta, ma" "\n\t"
        : "=r" (vl)
        : "r" (n));

      __asm__ __volatile__ (
        "vlseg4e32.v v0, %0" "\n\t"
        "vlseg4e32.v v8, %1" "\n\t"

        "vfadd.vv v16, v0, v4" "\n\t"   // xr[0] + xr[2]
        "vfadd.vv v18, v2, v6" "\n\t"   // xr[1] + xr[3]
        "vfsub.vv v20, v0, v4" "\n\t"   // xr[0] - xr[2]
        "vfsub.vv v22, v10, v14" "\n\t" // xi[1] - xi[3]

        "vfsub.vv v30, v6, v2" "\n\t"   // xr[3] - xr[1]
        "vfadd.vv v24, v8, v12" "\n\t"  // xi[0] + xi[2]
        "vfadd.vv v26, v10, v14" "\n\t" // xi[1] + xi[3]
        "vfsub.vv v28, v8, v12" "\n\t"  // xi[0] - xi[2]

        "vsseg4e32.v v16, %0" "\n\t"
        "vsseg4e32.v v24, %1" "\n\t"
        :
        : "A" (*xr), "A" (*xi));

        n -= vl;
        xr += 4 * vl;
        xi += 4 * vl;
    } while (n > 0);

    /* Stage M-1 */
    xr = Xr;
    xi = Xi;
    n = N / 2;
    do {
      size_t vl;
      __asm__ __volatile__ (
        "vsetvli %0, %1, e32, m4, ta, ma" "\n\t"
        : "=r" (vl)
        : "r" (n));

      __asm__ __volatile__ (
        "vlseg2e32.v v0, %0" "\n\t"
        "vlseg2e32.v v8, %1" "\n\t"

        "vfadd.vv v16, v0, v4" "\n\t"  // xr[0] + xr[1]
        "vfsub.vv v20, v0, v4" "\n\t"  // xr[0] - xr[1]
        "vfadd.vv v24, v8, v12" "\n\t" // xi[0] + xi[1]
        "vfsub.vv v28, v8, v12" "\n\t" // xi[0] - xi[1]

        "vsseg2e32.v v16, %0" "\n\t"
        "vsseg2e32.v v24, %1" "\n\t"
        :
        : "A" (*xr), "A" (*xi));

      n -= vl;
      xr += 2 * vl;
      xi += 2 * vl;
    } while (n > 0);
  }

  /* Bit-reversal unscrambler */
  {
    size_t i, j, b;
    size_t N1, N2;
    N1 = N - 1;
    N2 = N >> 1;
    for (i = 0, j = 0; i < N1; i++) {
      if (i < j) {
        float z;
        z = Xr[j];
        Xr[j] = Xr[i];
        Xr[i] = z;

        z = Xi[j];
        Xi[j] = Xi[i];
        Xi[i] = z;
      }
      b = ~i & (i + 1);
      b = N2 / b;
      j ^= N1 & ~(b - 1);
    }
  }
}
