// See LICENSE for license details.

//**************************************************************************
// Vectorized 2D 3x3 convolution
//--------------------------------------------------------------------------

    .text
    .balign 4

    .global vec_conv
/*
 * Calling convention:
 *     a0: size_t rows
 *     a1: size_t cols
 *     a2: size_t a_stride
 *     a3: size_t b_stride
 *     a4: const float *k
 *     a5: const float *a
 *	   a6: float *b
 */

#define rows		a0
#define cols	 	a1
#define a_stride	a2
#define b_stride 	a3
#define k			a4
#define a			a5
#define b			a6

#define ap			t0
#define bp			t1
#define vlen		t2
#define row_count	t3
#define VLEN_stride t4
#define ap_4		t5
#define ap_8		t6

#define row_check   s0
#define rows_odd    s1

#define k0			ft0
#define k1			ft1
#define k2			ft2
#define k3			ft3
#define k4			ft4
#define k5			ft5
#define k6			ft6
#define k7			ft7
#define k8			ft8

#define vload0		v0
#define vload1		v4
#define vload2		v8
#define vrow0		v16
#define vrow1		v20

#define FRAMESIZE 32

vec_conv:
    addi sp, sp, -FRAMESIZE
    sd s0, 0(sp)
    sd s1, 8(sp)

	# load the kernel into scalar registers
	flw k0, 0(k)
	flw k1, 4(k)
	flw k2, 8(k)
	flw k3, 12(k)
	flw k4, 16(k)
	flw k5, 20(k)
	flw k6, 24(k)
	flw k7, 28(k)
	flw k8, 32(k)

	slli a_stride, a_stride, 2
	slli b_stride, b_stride, 2

    mv row_check, rows
    addi row_check, row_check, -2

    andi rows_odd, rows, 1

# Prolog
loop_prolog:
    mv ap, a
    addi ap_4, ap, 4
    addi ap_8, ap, 8
    mv bp, b
    mv row_count, row_check

    vsetvli vlen, cols, e32, m4, ta, ma
    slli VLEN_stride, vlen, 2

    # Load the first row and compute horizontal
    vle32.v vload0, (ap)
    vfmul.vf vrow0, vload0, k0
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow0, k1, vload1
    vle32.v vload2, (ap_8)
    vfmacc.vf vrow0, k2, vload2

    add ap, ap, a_stride
    addi ap_4, ap, 4
    addi ap_8, ap, 8

    # Load the second row and compute horizontal
    vle32.v vload0, (ap)
    vfmacc.vf vrow0, k3, vload0
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow0, k4, vload1
    vle32.v vload2, (ap_8)
    vfmacc.vf vrow0, k5, vload2
    add ap, ap, a_stride

    # Load the third row
    vfmul.vf vrow1, vload0, k0
    vle32.v vload0, (ap)
    addi ap_4, ap, 4
    vfmacc.vf vrow1, k1, vload1
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow1, k2, vload2
    addi ap_8, ap, 8
    vle32.v vload2, (ap_8)
    
# Main Loop
conv_loop:
    vfmacc.vf vrow0, k6, vload0
	add ap, ap, a_stride
    vfmacc.vf vrow0, k7, vload1
	addi ap_4, ap, 4
    vfmacc.vf vrow0, k8, vload2
	addi ap_8, ap, 8

    vse32.v vrow0, (bp)
    
    vfmacc.vf vrow1, k3, vload0
    vfmacc.vf vrow1, k4, vload1
    vfmacc.vf vrow1, k5, vload2

    vfmul.vf vrow0, vload0, k0
    vle32.v vload0, (ap)
    vfmacc.vf vrow0, k1, vload1
    vle32.v vload1, (ap_4)
	add bp, bp, b_stride
    vfmacc.vf vrow0, k2, vload2

    vle32.v vload2, (ap_8)

    vfmacc.vf vrow1, k6, vload0
	add ap, ap, a_stride
    vfmacc.vf vrow1, k7, vload1
	addi ap_4, ap, 4
    vfmacc.vf vrow1, k8, vload2
	addi ap_8, ap, 8

    vfmacc.vf vrow0, k3, vload0
    vfmacc.vf vrow0, k4, vload1
    vfmacc.vf vrow0, k5, vload2

    vse32.v vrow1, (bp) 

    vfmul.vf vrow1, vload0, k0
    vle32.v vload0, (ap)
    vfmacc.vf vrow1, k1, vload1
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow1, k2, vload2
    vle32.v vload2, (ap_8)

	add bp, bp, b_stride
	addi row_count, row_count, -2

	bgtz row_count, conv_loop

epilog:
    vfmacc.vf vrow0, k6, vload0 
    vfmacc.vf vrow0, k7, vload1
    vfmacc.vf vrow0, k8, vload2
    vse32.v vrow0, (bp)

    bnez rows_odd, row_loop_complete

    vfmacc.vf vrow1, k3, vload0
    vfmacc.vf vrow1, k4, vload1
    vfmacc.vf vrow1, k5, vload2

	add ap, ap, a_stride
	addi ap_4, ap, 4
	addi ap_8, ap, 8
	add bp, bp, b_stride

    vle32.v vload0, (ap)
    vfmacc.vf vrow1, k6, vload0
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow1, k7, vload1
    vle32.v vload2, (ap_8)
    vfmacc.vf vrow1, k8, vload2

    vse32.v vrow1, (bp)
    
row_loop_complete:
	add a, a, VLEN_stride
	add b, b, VLEN_stride

	sub cols, cols, vlen
	bnez cols, loop_prolog

exit:
    ld s0, 0(sp)
    ld s1, 8(sp)
    addi sp, sp, FRAMESIZE

    ret
