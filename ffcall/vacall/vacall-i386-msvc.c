#ifdef _MSC_VER
#include "vacall.h"
#endif
#include "asmi386.h"
TEXT()
	ALIGN(4)
GLOBL(C(vacall))
FUNBEGIN(vacall)
	INSN2(sub,l	,NUM(36),R(esp))
	INSN2(mov,l	,NUM(0),X4 MEM(esp))
	INSN2(lea,l	,X4 MEM_DISP(esp,40),R(ecx))
	INSN2(mov,l	,R(ecx),X4 MEM_DISP(esp,4))
	INSN2(mov,l	,NUM(0),X4 MEM_DISP(esp,8))
	INSN2(mov,l	,NUM(0),X4 MEM_DISP(esp,12))
	INSN2(mov,l	,R(ebx),X4 MEM_DISP(esp,32))
	INSN2(mov,l	,R(esp),R(edx))
	INSN1(push,l	,R(edx))
	INSN2(mov,l	,C(vacall_function),R(edx))
	INSN1(call,_	,INDIR(R(edx)))
	INSN2(add,l	,NUM(4),R(esp))
	INSN2(mov,l	,X4 MEM_DISP(esp,12),R(edx))
	INSN2(cmp,l	,NUM(15),R(edx))
	INSN1(ja,_	,L2)
#ifdef _MSC_VER
	INSN2(lea,l	,MEM_DISP_SHINDEX0(L33+8,edx,8),R(edx))
	INSN2(mov,l	,X4 MEM_DISP(edx,-4),R(edx))
	INSN1(jmp,_	,INDIR(R(edx)))
#else
	INSN1(jmp,_	,INDIR(MEM_DISP_SHINDEX0(L33,edx,4)))
#endif
L33:
#ifdef _MSC_VER
	nop
	nop
	push L2
#else
	.long L2
#endif
#ifdef _MSC_VER
	nop
	nop
	push L4
#else
	.long L4
#endif
#ifdef _MSC_VER
	nop
	nop
	push L5
#else
	.long L5
#endif
#ifdef _MSC_VER
	nop
	nop
	push L6
#else
	.long L6
#endif
#ifdef _MSC_VER
	nop
	nop
	push L7
#else
	.long L7
#endif
#ifdef _MSC_VER
	nop
	nop
	push L8
#else
	.long L8
#endif
#ifdef _MSC_VER
	nop
	nop
	push L17
#else
	.long L17
#endif
#ifdef _MSC_VER
	nop
	nop
	push L17
#else
	.long L17
#endif
#ifdef _MSC_VER
	nop
	nop
	push L17
#else
	.long L17
#endif
#ifdef _MSC_VER
	nop
	nop
	push L17
#else
	.long L17
#endif
#ifdef _MSC_VER
	nop
	nop
	push L14
#else
	.long L14
#endif
#ifdef _MSC_VER
	nop
	nop
	push L14
#else
	.long L14
#endif
#ifdef _MSC_VER
	nop
	nop
	push L15
#else
	.long L15
#endif
#ifdef _MSC_VER
	nop
	nop
	push L16
#else
	.long L16
#endif
#ifdef _MSC_VER
	nop
	nop
	push L17
#else
	.long L17
#endif
#ifdef _MSC_VER
	nop
	nop
	push L18
#else
	.long L18
#endif
L4:
L5:
	INSN2MOVX(movs,b	,X1 MEM_DISP(esp,24),R(eax))
	INSN1(jmp,_	,L2)
L6:
	INSN2MOVX(movz,b	,X1 MEM_DISP(esp,24),R(eax))
	INSN1(jmp,_	,L2)
L7:
	INSN2MOVX(movs,w	,X2 MEM_DISP(esp,24),R(eax))
	INSN1(jmp,_	,L2)
L8:
	INSN2MOVX(movz,w	,X2 MEM_DISP(esp,24),R(eax))
	INSN1(jmp,_	,L2)
L14:
	INSN2(mov,l	,X4 MEM_DISP(esp,24),R(eax))
	INSN2(mov,l	,X4 MEM_DISP(esp,28),R(edx))
	INSN1(jmp,_	,L2)
L15:
	INSN1(fld,s	,X4 MEM_DISP(esp,24))
	INSN1(jmp,_	,L2)
L16:
	INSN1(fld,l	,X8 MEM_DISP(esp,24))
	INSN1(jmp,_	,L2)
L17:
	INSN2(mov,l	,X4 MEM_DISP(esp,24),R(eax))
	INSN1(jmp,_	,L2)
L18:
	INSN2(mov,l	,X4 MEM(esp),R(edx))
	INSN2(test,b	,NUM(1),R(dl))
	INSN1(jne,_	,L36)
	INSN2(test,b	,NUM(2),R(dh))
	INSN1(je,_	,L21)
	INSN2(mov,l	,X4 MEM_DISP(esp,16),R(edx))
	INSN2(cmp,l	,NUM(2),R(edx))
	INSN1(je,_	,L25)
	INSN1(ja,_	,L30)
	INSN2(cmp,l	,NUM(1),R(edx))
	INSN1(je,_	,L23)
	INSN1(jmp,_	,L21)
L30:
	INSN2(cmp,l	,NUM(4),R(edx))
	INSN1(je,_	,L26)
	INSN2(cmp,l	,NUM(8),R(edx))
	INSN1(je,_	,L27)
	INSN1(jmp,_	,L21)
L23:
	INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
	INSN2MOVX(movz,b	,X1 MEM(edx),R(eax))
	INSN1(jmp,_	,L2)
L25:
	INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
	INSN2MOVX(movz,w	,X2 MEM(edx),R(eax))
	INSN1(jmp,_	,L2)
L26:
	INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
	INSN2(mov,l	,X4 MEM(edx),R(eax))
	INSN1(jmp,_	,L2)
L27:
	INSN2(mov,l	,X4 MEM_DISP(esp,8),R(edx))
	INSN2(mov,l	,X4 MEM(edx),R(eax))
	INSN2(mov,l	,X4 MEM_DISP(edx,4),R(edx))
	INSN1(jmp,_	,L2)
L21:
	INSN2(mov,l	,X4 MEM(esp),R(edx))
	INSN2(test,b	,NUM(24),R(dl))
	INSN1(jne,_	,L31)
	INSN2(add,l	,NUM(36),R(esp))
	ret NUM(4)
L31:
	INSN2(test,b	,NUM(16),R(dl))
	INSN1(je,_	,L2)
L36:
	INSN2(mov,l	,X4 MEM_DISP(esp,8),R(eax))
L2:
	INSN2(test,b	,NUM(1),X1 MEM_DISP(esp,1))
	INSN1(je,_	,L35)
	INSN2(mov,l	,X4 MEM_DISP(esp,36),R(ecx))
	INSN2(mov,l	,X4 MEM_DISP(esp,4),R(esp))
	INSN1(jmp,_	,INDIR(R(ecx)))
L35:
	INSN2(add,l	,NUM(36),R(esp))
	ret
FUNEND()

