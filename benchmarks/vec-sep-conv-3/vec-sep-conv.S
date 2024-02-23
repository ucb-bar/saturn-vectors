// See LICENSE for license details.

//**************************************************************************
// Vectorized 2D separable convolution
//--------------------------------------------------------------------------

    .text
    .balign 4

    .global vec_sep_conv
/*
 * Calling convention:
 *     a0: size_t rows
 *     a1: size_t cols
 *     a2: size_t a_stride
 *     a3: size_t b_stride
 *     a4: const float *kw
 *     a5: const float *kh
 *     a6: const float *a
 *	   a7: float *b
 */

#define rows		a0
#define cols	 	a1
#define a_stride	a2
#define b_stride 	a3
#define kw			a4
#define kh			a5
#define a			a6
#define b			a7

#define ap			t0
#define bp			t1
#define vlen		t2
#define row_count	t3
#define VLEN_stride t4
#define ap_4		t5
#define ap_8		t6

#define row_check   s0
#define rows_odd    s1

#define kw0			ft0
#define kw1			ft1
#define kw2			ft2
#define kh0			ft3
#define kh1			ft4
#define kh2			ft5

#define vload0		v0
#define vload1		v4
#define vload2		v8
#define vrow0		v16
#define vrow1		v20
#define vtmp		v24

#define FRAMESIZE 32

vec_sep_conv:
    addi sp, sp, -FRAMESIZE
    sd s0, 0(sp)
    sd s1, 8(sp)

	# load the kernel into scalar registers
	flw kw0, 0(kw)
	flw kw1, 4(kw)
	flw kw2, 8(kw)
	flw kh0, 0(kh)
	flw kh1, 4(kh)
	flw kh2, 8(kh)

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
    vfmul.vf vrow0, vload0, kw0
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow0, kw1, vload1
    vle32.v vload2, (ap_8)
    vfmacc.vf vrow0, kw2, vload2

    add ap, ap, a_stride
    addi ap_4, ap, 4
    addi ap_8, ap, 8

    # Load the second row and compute horizontal
    vle32.v vload0, (ap)
    vfmul.vf vrow1, vload0, kw0
    vle32.v vload1, (ap_4)
    vfmacc.vf vrow1, kw1, vload1
    vle32.v vload2, (ap_8)
    vfmacc.vf vrow1, kw2, vload2

    add ap, ap, a_stride
    addi ap_4, ap, 4
    addi ap_8, ap, 8

    # Begin the vertical computation with the first and second rows
	vfmul.vf vrow0, vrow0, kh0
	vfmacc.vf vrow0, kh1, vrow1
	vfmul.vf vrow1, vrow1, kh0

    # Load the third row and compute horizontal
    vle32.v vload0, (ap)
    vfmul.vf vtmp, vload0, kw0
    vle32.v vload1, (ap_4)
    vfmacc.vf vtmp, kw1, vload1
    vle32.v vload2, (ap_8)
    vfmacc.vf vtmp, kw2, vload2
    
# Main Loop
conv_loop:

	add ap, ap, a_stride
	addi ap_4, ap, 4
	addi ap_8, ap, 8
    
    vle32.v vload0, (ap)
    vfmacc.vf vrow0, kh2, vtmp
    vle32.v vload1, (ap_4)
    vse32.v vrow0, (bp)
    vle32.v vload2, (ap_8)

    vfmacc.vf vrow1, kh1, vtmp
    vfmul.vf vrow0, vtmp, kh0

	add ap, ap, a_stride
    vfmul.vf vtmp, vload0, kw0
	addi ap_4, ap, 4
    vle32.v vload0, (ap)
    vfmacc.vf vtmp, kw1, vload1
	addi ap_8, ap, 8
    vle32.v vload1, (ap_4)
    vfmacc.vf vtmp, kw2, vload2
    vle32.v vload2, (ap_8)
	add bp, bp, b_stride

    vfmacc.vf vrow1, kh2, vtmp
    vfmacc.vf vrow0, kh1, vtmp
    vse32.v vrow1, (bp)
    vfmul.vf vrow1, vtmp, kh0

    vfmul.vf vtmp, vload0, kw0
	add bp, bp, b_stride
    vfmacc.vf vtmp, kw1, vload1
	addi row_count, row_count, -2
    vfmacc.vf vtmp, kw2, vload2


	bgtz row_count, conv_loop

epilog:
    vfmacc.vf vrow0, kh2, vtmp 
    vse32.v vrow0, (bp)

    bnez rows_odd, row_loop_complete

    vfmacc.vf vrow1, kh1, vtmp

	add ap, ap, a_stride
	addi ap_4, ap, 4
	addi ap_8, ap, 8
	add bp, bp, b_stride

    vle32.v vload0, (ap)
    vle32.v vload1, (ap_4)
    vle32.v vload2, (ap_8)

    vfmul.vf vtmp, vload0, kw0
    vfmacc.vf vtmp, kw1, vload1
    vfmacc.vf vtmp, kw2, vload2

    vfmacc.vf vrow1, kh2, vtmp
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
