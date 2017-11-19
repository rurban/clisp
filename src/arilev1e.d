/* extern declarations for ARILEV1.D */

BEGIN_DECLS

#ifdef COPY_LOOPS

#define copy_loop_up asm_copy_loop_up
extern uintD* copy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count);

#define copy_loop_down asm_copy_loop_down
extern uintD* copy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);

#endif

#ifdef FILL_LOOPS

#define fill_loop_up asm_fill_loop_up
extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);

#define fill_loop_down asm_fill_loop_down
extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);

#endif

#ifdef CLEAR_LOOPS

#define clear_loop_up asm_clear_loop_up
extern uintD* clear_loop_up (uintD* destptr, uintC count);

#define clear_loop_down asm_clear_loop_down
extern uintD* clear_loop_down (uintD* destptr, uintC count);

#endif

#ifdef LOG_LOOPS

#define or_loop_up asm_or_loop_up
extern void or_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define xor_loop_up asm_xor_loop_up
extern void xor_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define and_loop_up asm_and_loop_up
extern void and_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define eqv_loop_up asm_eqv_loop_up
extern void eqv_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define nand_loop_up asm_nand_loop_up
extern void nand_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define nor_loop_up asm_nor_loop_up
extern void nor_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define andc2_loop_up asm_andc2_loop_up
extern void andc2_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define orc2_loop_up asm_orc2_loop_up
extern void orc2_loop_up (uintD* xptr, const uintD* yptr, uintC count);

#define not_loop_up asm_not_loop_up
extern void not_loop_up (uintD* xptr, uintC count);

#endif

#ifdef TEST_LOOPS

#define and_test_loop_up asm_and_test_loop_up
extern /*bool*/int and_test_loop_up (const uintD* xptr, const uintD* yptr, uintC count);

#define test_loop_up asm_test_loop_up
extern /*bool*/int test_loop_up (const uintD* ptr, uintC count);

#define compare_loop_up asm_compare_loop_up
extern signean compare_loop_up (const uintD* xptr, const uintD* yptr, uintC count);

#endif

#ifdef ADDSUB_LOOPS

#define add_loop_down asm_add_loop_down
extern uintD add_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count);

#define addto_loop_down asm_addto_loop_down
extern uintD addto_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);

#define inc_loop_down asm_inc_loop_down
extern uintD inc_loop_down (uintD* ptr, uintC count);

#define sub_loop_down asm_sub_loop_down
extern uintD sub_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count);

#define subx_loop_down asm_subx_loop_down
extern uintD subx_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);

#define subfrom_loop_down asm_subfrom_loop_down
extern uintD subfrom_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);

#define dec_loop_down asm_dec_loop_down
extern uintD dec_loop_down (uintD* ptr, uintC count);

#define neg_loop_down asm_neg_loop_down
extern uintD neg_loop_down (uintD* ptr, uintC count);

#endif

#ifdef SHIFT_LOOPS

#define shift1left_loop_down asm_shift1left_loop_down
extern uintD shift1left_loop_down (uintD* ptr, uintC count);

#define shiftleft_loop_down asm_shiftleft_loop_down
extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);

#define shiftleftcopy_loop_down asm_shiftleftcopy_loop_down
extern uintD shiftleftcopy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count, uintC i);

#define shift1right_loop_up asm_shift1right_loop_up
extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry);

#define shiftright_loop_up asm_shiftright_loop_up
extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);

#define shiftrightsigned_loop_up asm_shiftrightsigned_loop_up
extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);

#define shiftrightcopy_loop_up asm_shiftrightcopy_loop_up
extern uintD shiftrightcopy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);

#endif

#ifdef MUL_LOOPS

#define mulusmall_loop_down asm_mulusmall_loop_down
extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);

#define mulu_loop_down asm_mulu_loop_down
extern void mulu_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

#define muluadd_loop_down asm_muluadd_loop_down
extern uintD muluadd_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

#define mulusub_loop_down asm_mulusub_loop_down
extern uintD mulusub_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

#endif

#ifdef DIV_LOOPS

#define divu_loop_up asm_divu_loop_up
extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);

#define divucopy_loop_up asm_divucopy_loop_up
extern uintD divucopy_loop_up (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

#endif

END_DECLS

