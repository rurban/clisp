/* Definitions for ARILEV1.D for debugging the definitions in ari_asm_*.d. */

#ifdef COPY_LOOPS

#undef copy_loop_up
global unsigned int copy_loop_up_counter;
local uintD* copy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count)
{
  copy_loop_up_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  portable_copy_loop_up(sourceptr,correct_dest,count);
  var uintD* correct_result = destptr + count;
  var uintD* result = asm_copy_loop_up(sourceptr,destptr,count);
  if (portable_compare_loop_up(destptr,correct_dest,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef copy_loop_down
global unsigned int copy_loop_down_counter;
local uintD* copy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count)
{
  copy_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  portable_copy_loop_down(sourceptr,correct_dest+count,count);
  var uintD* correct_result = destptr - count;
  var uintD* result = asm_copy_loop_down(sourceptr,destptr,count);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#endif

#ifdef FILL_LOOPS

#undef fill_loop_up
global unsigned int fill_loop_up_counter;
local uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler)
{
  fill_loop_up_counter++;
  var uintD* correct_result = destptr + count;
  var uintD* result = asm_fill_loop_up(destptr,count,filler);
  var uintC i;
  for (i = 0; i < count; i++) {
    if (destptr[i] != filler)
      abort();
  }
  if (result != correct_result)
    abort();
  return result;
}

#undef fill_loop_down
global unsigned int fill_loop_down_counter;
local uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler)
{
  fill_loop_down_counter++;
  var uintD* correct_result = destptr - count;
  var uintD* result = asm_fill_loop_down(destptr,count,filler);
  var uintC i;
  for (i = 0; i < count; i++) {
    if (destptr[-1-(sintP)i] != filler)
      abort();
  }
  if (result != correct_result)
    abort();
  return result;
}

#endif

#ifdef CLEAR_LOOPS

#undef clear_loop_up
global unsigned int clear_loop_up_counter;
local uintD* clear_loop_up (uintD* destptr, uintC count)
{
  clear_loop_up_counter++;
  var uintD* correct_result = destptr + count;
  var uintD* result = asm_clear_loop_up(destptr,count);
  if (portable_test_loop_up(destptr,count) || result != correct_result)
    abort();
  return result;
}

#undef clear_loop_down
global unsigned int clear_loop_down_counter;
local uintD* clear_loop_down (uintD* destptr, uintC count)
{
  clear_loop_down_counter++;
  var uintD* correct_result = destptr - count;
  var uintD* result = asm_clear_loop_down(destptr,count);
  if (portable_test_loop_up(destptr-count,count) || result != correct_result)
    abort();
  return result;
}

#endif

#ifdef LOG_LOOPS

#undef or_loop_up
global unsigned int or_loop_up_counter;
local void or_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  or_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = xptr[i] | yptr[i];
  asm_or_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef xor_loop_up
global unsigned int xor_loop_up_counter;
local void xor_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  xor_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = xptr[i] ^ yptr[i];
  asm_xor_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef and_loop_up
global unsigned int and_loop_up_counter;
local void and_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  and_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = xptr[i] & yptr[i];
  asm_and_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef eqv_loop_up
global unsigned int eqv_loop_up_counter;
local void eqv_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  eqv_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = ~(xptr[i] ^ yptr[i]);
  asm_eqv_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef nand_loop_up
global unsigned int nand_loop_up_counter;
local void nand_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  nand_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = ~(xptr[i] & yptr[i]);
  asm_nand_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef nor_loop_up
global unsigned int nor_loop_up_counter;
local void nor_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  nor_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = ~(xptr[i] | yptr[i]);
  asm_nor_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef andc2_loop_up
global unsigned int andc2_loop_up_counter;
local void andc2_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  andc2_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = xptr[i] & ~yptr[i];
  asm_andc2_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef orc2_loop_up
global unsigned int orc2_loop_up_counter;
local void orc2_loop_up (uintD* xptr, const uintD* yptr, uintC count)
{
  orc2_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = xptr[i] | ~yptr[i];
  asm_orc2_loop_up(xptr,yptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#undef not_loop_up
global unsigned int not_loop_up_counter;
local void not_loop_up (uintD* xptr, uintC count)
{
  not_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  var uintC i;
  for (i = 0; i < count; i++)
    correct[i] = ~xptr[i];
  asm_not_loop_up(xptr,count);
  if (portable_compare_loop_up(xptr,correct,count) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
}

#endif

#ifdef TEST_LOOPS

#undef and_test_loop_up
global unsigned int and_test_loop_up_counter;
local /*bool*/int and_test_loop_up (const uintD* xptr, const uintD* yptr, uintC count)
{
  and_test_loop_up_counter++;
  var int correct_result = portable_and_test_loop_up(xptr,yptr,count);
  var int result = asm_and_test_loop_up(xptr,yptr,count);
  if ((result != 0) != (correct_result != 0))
    abort();
  return result;
}

#undef test_loop_up
global unsigned int test_loop_up_counter;
local /*bool*/int test_loop_up (const uintD* ptr, uintC count)
{
  test_loop_up_counter++;
  var int correct_result = portable_test_loop_up(ptr,count);
  var int result = asm_test_loop_up(ptr,count);
  if ((result != 0) != (correct_result != 0))
    abort();
  return result;
}

#undef compare_loop_up
global unsigned int compare_loop_up_counter;
local signean compare_loop_up (const uintD* xptr, const uintD* yptr, uintC count)
{
  compare_loop_up_counter++;
  var int correct_result = portable_compare_loop_up(xptr,yptr,count);
  var int result = asm_compare_loop_up(xptr,yptr,count);
  if (result != correct_result)
    abort();
  return result;
}

#endif

#ifdef ADDSUB_LOOPS

#undef add_loop_down
global unsigned int add_loop_down_counter;
local uintD add_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count)
{
  add_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  var uintD correct_result = portable_add_loop_down(sourceptr1,sourceptr2,correct_dest+count,count);
  var uintD result = asm_add_loop_down(sourceptr1,sourceptr2,destptr,count);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef addto_loop_down
global unsigned int addto_loop_down_counter;
local uintD addto_loop_down (const uintD* sourceptr, uintD* destptr, uintC count)
{
  addto_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  portable_copy_loop_down(destptr,correct_dest+count,count);
  var uintD correct_result = portable_addto_loop_down(sourceptr,correct_dest+count,count);
  var uintD result = asm_addto_loop_down(sourceptr,destptr,count);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef inc_loop_down
global unsigned int inc_loop_down_counter;
local uintD inc_loop_down (uintD* ptr, uintC count)
{
  inc_loop_down_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_down(ptr,correct+count,count);
  var uintD correct_result = portable_inc_loop_down(correct+count,count);
  var uintD result = asm_inc_loop_down(ptr,count);
  if (portable_compare_loop_up(ptr-count,correct,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef sub_loop_down
global unsigned int sub_loop_down_counter;
local uintD sub_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count)
{
  sub_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  var uintD correct_result = portable_sub_loop_down(sourceptr1,sourceptr2,correct_dest+count,count);
  var uintD result = asm_sub_loop_down(sourceptr1,sourceptr2,destptr,count);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef subx_loop_down
global unsigned int subx_loop_down_counter;
local uintD subx_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count, uintD carry)
{
  subx_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  var uintD correct_result = portable_subx_loop_down(sourceptr1,sourceptr2,correct_dest+count,count,carry);
  var uintD result = asm_subx_loop_down(sourceptr1,sourceptr2,destptr,count,carry);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);

  return result;
}

#undef subfrom_loop_down
global unsigned int subfrom_loop_down_counter;
local uintD subfrom_loop_down (const uintD* sourceptr, uintD* destptr, uintC count)
{
  subfrom_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  portable_copy_loop_down(destptr,correct_dest+count,count);
  var uintD correct_result = portable_subfrom_loop_down(sourceptr,correct_dest+count,count);
  var uintD result = asm_subfrom_loop_down(sourceptr,destptr,count);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef dec_loop_down
global unsigned int dec_loop_down_counter;
local uintD dec_loop_down (uintD* ptr, uintC count)
{
  dec_loop_down_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_down(ptr,correct+count,count);
  var uintD correct_result = portable_dec_loop_down(correct+count,count);
  var uintD result = asm_dec_loop_down(ptr,count);
  if (portable_compare_loop_up(ptr-count,correct,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef neg_loop_down
global unsigned int neg_loop_down_counter;
local uintD neg_loop_down (uintD* ptr, uintC count)
{
  neg_loop_down_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_down(ptr,correct+count,count);
  var uintD correct_result = portable_neg_loop_down(correct+count,count);
  var uintD result = asm_neg_loop_down(ptr,count);
  if (portable_compare_loop_up(ptr-count,correct,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#endif

#ifdef SHIFT_LOOPS

#undef shift1left_loop_down
global unsigned int shift1left_loop_down_counter;
local uintD shift1left_loop_down (uintD* ptr, uintC count)
{
  shift1left_loop_down_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_down(ptr,correct+count,count);
  var uintD correct_result = portable_shift1left_loop_down(correct+count,count);
  var uintD result = asm_shift1left_loop_down(ptr,count);
  if (portable_compare_loop_up(ptr-count,correct,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef shiftleft_loop_down
global unsigned int shiftleft_loop_down_counter;
local uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry)
{
  shiftleft_loop_down_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_down(ptr,correct+count,count);
  var uintD correct_result = portable_shiftleft_loop_down(correct+count,count,i,carry);
  var uintD result = asm_shiftleft_loop_down(ptr,count,i,carry);
  if (portable_compare_loop_up(ptr-count,correct,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef shiftleftcopy_loop_down
global unsigned int shiftleftcopy_loop_down_counter;
local uintD shiftleftcopy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count, uintC i)
{
  shiftleftcopy_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  var uintD correct_result = portable_shiftleftcopy_loop_down(sourceptr,correct_dest+count,count,i);
  var uintD result = asm_shiftleftcopy_loop_down(sourceptr,destptr,count,i);
  if (portable_compare_loop_up(destptr-count,correct_dest,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef shift1right_loop_up
global unsigned int shift1right_loop_up_counter;
local uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry)
{
  shift1right_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_up(ptr,correct,count);
  var uintD correct_result = portable_shift1right_loop_up(correct,count,carry);
  var uintD result = asm_shift1right_loop_up(ptr,count,carry);
  if (portable_compare_loop_up(ptr,correct,count) != 0 || (result != 0) != (correct_result != 0))
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef shiftright_loop_up
global unsigned int shiftright_loop_up_counter;
local uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i)
{
  shiftright_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_up(ptr,correct,count);
  var uintD correct_result = portable_shiftright_loop_up(correct,count,i);
  var uintD result = asm_shiftright_loop_up(ptr,count,i);
  if (portable_compare_loop_up(ptr,correct,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef shiftrightsigned_loop_up
global unsigned int shiftrightsigned_loop_up_counter;
local uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i)
{
  shiftrightsigned_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,count);
  portable_copy_loop_up(ptr,correct,count);
  var uintD correct_result = portable_shiftrightsigned_loop_up(correct,count,i);
  var uintD result = asm_shiftrightsigned_loop_up(ptr,count,i);
  if (portable_compare_loop_up(ptr,correct,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef shiftrightcopy_loop_up
global unsigned int shiftrightcopy_loop_up_counter;
local uintD shiftrightcopy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry)
{
  shiftrightcopy_loop_up_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,count);
  var uintD correct_result = portable_shiftrightcopy_loop_up(sourceptr,correct_dest,count,i,carry);
  var uintD result = asm_shiftrightcopy_loop_up(sourceptr,destptr,count,i,carry);
  if (portable_compare_loop_up(destptr,correct_dest,count) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#endif

#ifdef MUL_LOOPS

#undef mulusmall_loop_down
global unsigned int mulusmall_loop_down_counter;
local uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit)
{
  mulusmall_loop_down_counter++;
  var DYNAMIC_ARRAY(correct,uintD,len);
  portable_copy_loop_down(ptr,correct+len,len);
  var uintD correct_result = portable_mulusmall_loop_down(digit,correct+len,len,newdigit);
  var uintD result = asm_mulusmall_loop_down(digit,ptr,len,newdigit);
  if (portable_compare_loop_up(ptr-len,correct,len) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef mulu_loop_down
global unsigned int mulu_loop_down_counter;
local void mulu_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len)
{
  mulu_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,len);
  portable_mulu_loop_down(digit,sourceptr,correct_dest+len,len);
  asm_mulu_loop_down(digit,sourceptr,destptr,len);
  if (portable_compare_loop_up(destptr-len,correct_dest,len) != 0)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
}

#undef muluadd_loop_down
global unsigned int muluadd_loop_down_counter;
local uintD muluadd_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len)
{
  muluadd_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,len);
  portable_copy_loop_down(destptr,correct_dest+len,len);
  var uintD correct_result = portable_muluadd_loop_down(digit,sourceptr,correct_dest+len,len);
  var uintD result = asm_muluadd_loop_down(digit,sourceptr,destptr,len);
  if (portable_compare_loop_up(destptr-len,correct_dest,len) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#undef mulusub_loop_down
global unsigned int mulusub_loop_down_counter;
local uintD mulusub_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len)
{
  mulusub_loop_down_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,len);
  portable_copy_loop_down(destptr,correct_dest+len,len);
  var uintD correct_result = portable_mulusub_loop_down(digit,sourceptr,correct_dest+len,len);
  var uintD result = asm_mulusub_loop_down(digit,sourceptr,destptr,len);
  if (portable_compare_loop_up(destptr-len,correct_dest,len) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#endif

#ifdef DIV_LOOPS

#undef divu_loop_up
global unsigned int divu_loop_up_counter;
local uintD divu_loop_up (uintD digit, uintD* ptr, uintC len)
{
  divu_loop_up_counter++;
  var DYNAMIC_ARRAY(correct,uintD,len);
  portable_copy_loop_up(ptr,correct,len);
  var uintD correct_result = portable_divu_loop_up(digit,correct,len);
  var uintD result = asm_divu_loop_up(digit,ptr,len);
  if (portable_compare_loop_up(ptr,correct,len) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct);
  return result;
}

#undef divucopy_loop_up
global unsigned int divucopy_loop_up_counter;
local uintD divucopy_loop_up (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len)
{
  divucopy_loop_up_counter++;
  var DYNAMIC_ARRAY(correct_dest,uintD,len);
  var uintD correct_result = portable_divucopy_loop_up(digit,sourceptr,correct_dest,len);
  var uintD result = asm_divucopy_loop_up(digit,sourceptr,destptr,len);
  if (portable_compare_loop_up(destptr,correct_dest,len) != 0 || result != correct_result)
    abort();
  FREE_DYNAMIC_ARRAY(correct_dest);
  return result;
}

#endif

