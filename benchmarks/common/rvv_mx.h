// VTYPE options

#define LMUL_M1 "000"
#define LMUL_M2 "001"
#define LMUL_M4 "010"
#define LMUL_M8 "011"
#define LMUL_MF2 "111"
#define LMUL_MF4 "110"
#define LMUL_MF8 "101"

#define SEW_E8 "00"
#define SEW_E16 "01"
#define SEW_E32 "10"
#define SEW_E64 "11"

// Register names

#define V0 "x0"
#define V1 "x1"
#define V2 "x2"
#define V3 "x3"
#define V4 "x4"
#define V5 "x5"
#define V6 "x6"
#define V7 "x7"
#define V8 "x8"
#define V9 "x9"
#define V10 "x10"
#define V11 "x11"
#define V12 "x12"
#define V13 "x13"
#define V14 "x14"
#define V15 "x15"
#define V16 "x16"
#define V17 "x17"
#define V18 "x18"
#define V19 "x19"
#define V20 "x20"
#define V21 "x21"
#define V22 "x22"
#define V23 "x23"
#define V24 "x24"
#define V25 "x25"
#define V26 "x26"
#define V27 "x27"
#define V28 "x28"
#define V29 "x29"
#define V30 "x30"
#define V31 "x31"

#define X0 "x0"
#define X1 "x1"
#define X2 "x2"
#define X3 "x3"
#define X4 "x4"
#define X5 "x5"
#define X6 "x6"
#define X7 "x7"
#define X8 "x8"
#define X9 "x9"
#define X10 "x10"
#define X11 "x11"
#define X12 "x12"
#define X13 "x13"
#define X14 "x14"
#define X15 "x15"
#define X16 "x16"
#define X17 "x17"
#define X18 "x18"
#define X19 "x19"
#define X20 "x20"
#define X21 "x21"
#define X22 "x22"
#define X23 "x23"
#define X24 "x24"
#define X25 "x25"
#define X26 "x26"
#define X27 "x27"
#define X28 "x28"
#define X29 "x29"
#define X30 "x30"
#define X31 "x31"

#define F0 "x0"
#define F1 "x1"
#define F2 "x2"
#define F3 "x3"
#define F4 "x4"
#define F5 "x5"
#define F6 "x6"
#define F7 "x7"
#define F8 "x8"
#define F9 "x9"
#define F10 "x10"
#define F11 "x11"
#define F12 "x12"
#define F13 "x13"
#define F14 "x14"
#define F15 "x15"
#define F16 "x16"
#define F17 "x17"
#define F18 "x18"
#define F19 "x19"
#define F20 "x20"
#define F21 "x21"
#define F22 "x22"
#define F23 "x23"
#define F24 "x24"
#define F25 "x25"
#define F26 "x26"
#define F27 "x27"
#define F28 "x28"
#define F29 "x29"
#define F30 "x30"
#define F31 "x31"

// debug

#define STALL(n) \
	for (int __i = 0; __i < n; __i ++) { asm volatile("nop"); }

// vsetvli

#define VSETVLI_ALTFMT(vl, avl, sew, lmul, alt) \
	asm volatile(".insn i 0x57, 0x7, %0, %1, 0b000" #alt "000" sew lmul : "=r"(vl) : "r"(avl))

#define VSETVLI_ALTFMT_X0(avl, sew, lmul, alt) \
	asm volatile(".insn i 0x57, 0x7, zero, %0, 0b000" #alt "000" sew lmul :: "r"(avl))

// vector conversion

#define VFNCVTBF16_F_F_W(rd, vs2) \
	asm volatile(".insn r 0x57, 0x1, 0x25, " rd ", x29, " vs2)

#define VFWCVTBF16_F_F_V(rd, vs2) \
	asm volatile(".insn r 0x57, 0x1, 0x25, " rd ", x13, " vs2)

#define VFNCVTBF16_SAT_F_F_W(rd, vs2) \
	asm volatile(".insn r 0x57, 0x1, 0x25, " rd ", x31, " vs2)

// scalar conversion

#define FCVT_BF16_S(fd, fs1) \
	asm volatile(".insn r 0x53, 0x0, 0x22, " fd ", " fs1 ", x8")

#define FCVT_S_BF16(fd, fs1) \
	asm volatile(".insn r 0x53, 0x0, 0x20, " fd ", " fs1 ", x6")

// bdot

#define VDOTSET_VV(rd, as) \
	asm volatile(".insn r 0x77, 0x0, 0x00, " rd ", x0, " as)
#define VDOTSETZERO_VV(as) \
	asm volatile(".insn r 0x77, 0x0, 0x00, x0, x1, " as)
#define VDOTSETZEROBC_VV() \
	asm volatile(".insn r 0x77, 0x0, 0x00, x0, x3, x0")
#define VDOTWB_VV(rd, as) \
	asm volatile(".insn r 0x77, 0x0, 0x02, " rd ", x0, " as)
#define VQLDOTUA_VV(as, rs2, rs1) \
	asm volatile(".insn r 0x77, 0x0, 0x4c, " as ", " rs1 ", " rs2)
#define VQLDOTSA_VV(as, rs2, rs1) \
	asm volatile(".insn r 0x77, 0x0, 0x4e, " as ", " rs1 ", " rs2)
#define VQBDOTUA_VV(as, rs2, rs1) \
	asm volatile(".insn r 0x77, 0x0, 0x5c, " as ", " rs1 ", " rs2)
#define VQBDOTSA_VV(as, rs2, rs1) \
	asm volatile(".insn r 0x77, 0x0, 0x5e, " as ", " rs1 ", " rs2)

// opu

#define OPMVIN(md, vs2, rs1) \
	asm volatile(".insn r 0x57, 0x6, 0x55, " md ", %0, " vs2 : : "r"(rs1));
#define OPMVOUT(vd, ms2, rs1) \
	asm volatile(".insn r 0x57, 0x6, 0x5d, " vd ", %0, " ms2 : : "r"(rs1));
#define OPMVINBCAST(md, vs2) \
	asm volatile(".insn r 0x57, 0x6, 0x59, " md ", x0, " vs2);
#define OPMACC(md, vs2, vs1) \
	asm volatile(".insn r 0x57, 0x2, 0x51, " md ", " vs1 ", " vs2);
