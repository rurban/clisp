#include "asmi386.h"
TEXT()
	ALIGN(2)
GLOBL(C(__builtin_avcall))
FUNBEGIN(__builtin_avcall)
	INSN1(push,l	,R(ebp))
	INSN2(mov,l	,R(esp),R(ebp))
	INSN1(push,l	,R(esi))
	INSN1(push,l	,R(ebx))
	INSN2(mov,l	,X4 MEM_DISP(ebp,8),R(esi))
	INSN2(add,l	,NUM(-1024),R(esp))
	INSN2(mov,l	,R(esp),R(ecx))
	INSN2(mov,l	,X4 MEM_DISP(esi,20),R(eax))
	INSN2(add,l	,NUM(-32),R(eax))
	INSN2(sub,l	,R(esi),R(eax))
	INSN2(mov,l	,R(eax),R(edx))
	INSN2(sar,l	,NUM(2),R(edx))
	INSN2(xor,l	,R(ebx),R(ebx))
	INSN2(cmp,l	,R(edx),R(ebx))
	INSN1(jge,_	,L3)
L5:
	INSN2(mov,l	,X4 MEM_DISP_SHINDEX(esi,32,ebx,4),R(eax))
	INSN2(mov,l	,R(eax),X4 MEM_SHINDEX(ecx,ebx,4))
	INSN1(inc,l	,R(ebx))
	INSN2(cmp,l	,R(edx),R(ebx))
	INSN1(jl,_	,L5)
L3:
	INSN2(test,b	,NUM(8),X1 MEM_DISP(esi,4))
	INSN1(je,_	,L7)
	INSN2(cmp,l	,NUM(16),X4 MEM_DISP(esi,12))
	INSN1(jne,_	,L7)
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(ebx))
L7:
	INSN2(mov,l	,X4 MEM_DISP(esi,12),R(eax))
	INSN2(cmp,l	,NUM(13),R(eax))
	INSN1(je,_	,L9)
	INSN2(cmp,l	,NUM(14),R(eax))
	INSN1(je,_	,L10)
	INSN1(jmp,_	,L11)
L9:
	INSN2(mov,l	,X4 MEM(esi),R(eax))
	INSN1(call,_	,INDIR(R(eax)))
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN1(fstp,s	,X4 MEM(eax))
	INSN1(jmp,_	,L13)
L10:
	INSN2(mov,l	,X4 MEM(esi),R(eax))
	INSN1(call,_	,INDIR(R(eax)))
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN1(fstp,l	,X8 MEM(eax))
	INSN1(jmp,_	,L13)
L11:
	INSN2(mov,l	,X4 MEM(esi),R(eax))
	INSN1(call,_	,INDIR(R(eax)))
	INSN2(mov,l	,R(eax),R(ebx))
	INSN2(mov,l	,X4 MEM_DISP(esi,12),R(eax))
	INSN2(cmp,l	,NUM(16),R(eax))
	INSN1(ja,_	,L13)
#ifdef _MSC_VER
	INSN2(lea,l	,MEM_DISP_SHINDEX0(L55+8,eax,8),R(eax))
	INSN2(mov,l	,X4 MEM_DISP(eax,-4),R(eax))
	INSN1(jmp,_	,INDIR(R(eax)))
#else
	INSN1(jmp,_	,INDIR(MEM_DISP_SHINDEX0(L55,eax,4)))
#endif
L55:
#ifdef _MSC_VER
	nop
	nop
	push L49
#else
	.long L49
#endif
#ifdef _MSC_VER
	nop
	nop
	push L13
#else
	.long L13
#endif
#ifdef _MSC_VER
	nop
	nop
	push L47
#else
	.long L47
#endif
#ifdef _MSC_VER
	nop
	nop
	push L47
#else
	.long L47
#endif
#ifdef _MSC_VER
	nop
	nop
	push L47
#else
	.long L47
#endif
#ifdef _MSC_VER
	nop
	nop
	push L48
#else
	.long L48
#endif
#ifdef _MSC_VER
	nop
	nop
	push L48
#else
	.long L48
#endif
#ifdef _MSC_VER
	nop
	nop
	push L49
#else
	.long L49
#endif
#ifdef _MSC_VER
	nop
	nop
	push L49
#else
	.long L49
#endif
#ifdef _MSC_VER
	nop
	nop
	push L49
#else
	.long L49
#endif
#ifdef _MSC_VER
	nop
	nop
	push L49
#else
	.long L49
#endif
#ifdef _MSC_VER
	nop
	nop
	push L50
#else
	.long L50
#endif
#ifdef _MSC_VER
	nop
	nop
	push L50
#else
	.long L50
#endif
#ifdef _MSC_VER
	nop
	nop
	push L13
#else
	.long L13
#endif
#ifdef _MSC_VER
	nop
	nop
	push L13
#else
	.long L13
#endif
#ifdef _MSC_VER
	nop
	nop
	push L49
#else
	.long L49
#endif
#ifdef _MSC_VER
	nop
	nop
	push L30
#else
	.long L30
#endif
L30:
	INSN2(mov,l	,X4 MEM_DISP(esi,4),R(eax))
	INSN2(test,b	,NUM(1),R(al))
	INSN1(je,_	,L31)
	INSN2(mov,l	,X4 MEM_DISP(esi,16),R(eax))
	INSN2(cmp,l	,NUM(2),R(eax))
	INSN1(je,_	,L34)
	INSN1(ja,_	,L43)
	INSN2(cmp,l	,NUM(1),R(eax))
	INSN1(je,_	,L33)
	INSN1(jmp,_	,L37)
L43:
	INSN2(cmp,l	,NUM(4),R(eax))
	INSN1(je,_	,L35)
	INSN2(cmp,l	,NUM(8),R(eax))
	INSN1(je,_	,L36)
	INSN1(jmp,_	,L37)
L33:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(edx))
	INSN2(mov,b	,X1 MEM(ebx),R(al))
	INSN2(mov,b	,R(al),X1 MEM(edx))
	INSN1(jmp,_	,L13)
L34:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(edx))
	INSN2(mov,w	,X2 MEM(ebx),R(ax))
	INSN2(mov,w	,R(ax),X2 MEM(edx))
	INSN1(jmp,_	,L13)
L35:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(edx))
	INSN2(mov,l	,X4 MEM(ebx),R(eax))
	INSN2(mov,l	,R(eax),X4 MEM(edx))
	INSN1(jmp,_	,L13)
L36:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(edx))
	INSN2(mov,l	,X4 MEM(ebx),R(eax))
	INSN2(mov,l	,R(eax),X4 MEM(edx))
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(edx))
	INSN2(mov,l	,X4 MEM_DISP(ebx,4),R(eax))
	INSN2(mov,l	,R(eax),X4 MEM_DISP(edx,4))
	INSN1(jmp,_	,L13)
L37:
	INSN2(mov,l	,X4 MEM_DISP(esi,16),R(eax))
	INSN2(add,l	,NUM(3),R(eax))
	INSN2(mov,l	,R(eax),R(ecx))
	INSN2(shr,l	,NUM(2),R(ecx))
	INSN1(dec,l	,R(ecx))
	js L13
L40:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(edx))
	INSN2(mov,l	,X4 MEM_SHINDEX(ebx,ecx,4),R(eax))
	INSN2(mov,l	,R(eax),X4 MEM_SHINDEX(edx,ecx,4))
	INSN1(dec,l	,R(ecx))
	jns L40
	INSN1(jmp,_	,L13)
L31:
	INSN2(test,b	,NUM(2),R(ah))
	INSN1(je,_	,L13)
	INSN2(mov,l	,X4 MEM_DISP(esi,16),R(eax))
	INSN2(cmp,l	,NUM(2),R(eax))
	INSN1(je,_	,L48)
	INSN1(ja,_	,L53)
	INSN2(cmp,l	,NUM(1),R(eax))
	INSN1(je,_	,L47)
	INSN1(jmp,_	,L13)
L53:
	INSN2(cmp,l	,NUM(4),R(eax))
	INSN1(je,_	,L49)
	INSN2(cmp,l	,NUM(8),R(eax))
	INSN1(je,_	,L50)
	INSN1(jmp,_	,L13)
L47:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN2(mov,b	,R(bl),X1 MEM(eax))
	INSN1(jmp,_	,L13)
L48:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN2(mov,w	,R(bx),X2 MEM(eax))
	INSN1(jmp,_	,L13)
L49:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN2(mov,l	,R(ebx),X4 MEM(eax))
	INSN1(jmp,_	,L13)
L50:
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN2(mov,l	,R(ebx),X4 MEM(eax))
	INSN2(mov,l	,X4 MEM_DISP(esi,8),R(eax))
	INSN2(mov,l	,R(edx),X4 MEM_DISP(eax,4))
L13:
	INSN2(xor,l	,R(eax),R(eax))
	INSN2(lea,l	,X4 MEM_DISP(ebp,-8),R(esp))
	INSN1(pop,l	,R(ebx))
	INSN1(pop,l	,R(esi))
	INSN2(mov,l	,R(ebp),R(esp))
	INSN1(pop,l	,R(ebp))
	ret
FUNEND()

