#include "asmi386.h"
	.file	"vacall-i386.c"
gcc2_compiled.:
C(__gnu_compiled_c:)
.text
	.align 4
.globl C(vacall)
C(vacall:)
	subl $36,%esp
	movl $0,(%esp)
	leal 40(%esp),%ecx
	movl %ecx,4(%esp)
	movl $0,8(%esp)
	movl $0,12(%esp)
	movl %ebx,32(%esp)
	movl %esp,%edx
	pushl %edx
	movl C(vacall_function),%edx
	call *%edx
	addl $4,%esp
	movl 12(%esp),%edx
	cmpl $15,%edx
	ja L2
	jmp *L33(,%edx,4)
L33:
	.long L2
	.long L4
	.long L5
	.long L6
	.long L7
	.long L8
	.long L17
	.long L17
	.long L17
	.long L17
	.long L14
	.long L14
	.long L15
	.long L16
	.long L17
	.long L18
L4:
L5:
	movsbl 24(%esp),%eax
	jmp L2
L6:
	movzbl 24(%esp),%eax
	jmp L2
L7:
	movswl 24(%esp),%eax
	jmp L2
L8:
	movzwl 24(%esp),%eax
	jmp L2
L14:
	movl 24(%esp),%eax
	movl 28(%esp),%edx
	jmp L2
L15:
	flds 24(%esp)
	jmp L2
L16:
	fldl 24(%esp)
	jmp L2
L17:
	movl 24(%esp),%eax
	jmp L2
L18:
	movl (%esp),%edx
	testb $1,%dl
	jne L36
	testb $2,%dh
	je L21
	movl 16(%esp),%edx
	cmpl $2,%edx
	je L25
	ja L30
	cmpl $1,%edx
	je L23
	jmp L21
L30:
	cmpl $4,%edx
	je L26
	cmpl $8,%edx
	je L27
	jmp L21
L23:
	movl 8(%esp),%edx
	movzbl (%edx),%eax
	jmp L2
L25:
	movl 8(%esp),%edx
	movzwl (%edx),%eax
	jmp L2
L26:
	movl 8(%esp),%edx
	movl (%edx),%eax
	jmp L2
L27:
	movl 8(%esp),%edx
	movl (%edx),%eax
	movl 4(%edx),%edx
	jmp L2
L21:
	movl (%esp),%edx
	testb $24,%dl
	jne L31
	addl $36,%esp
	ret $4
L31:
	testb $16,%dl
	je L2
L36:
	movl 8(%esp),%eax
L2:
	testb $1,1(%esp)
	je L35
	movl 36(%esp),%ecx
	movl 4(%esp),%esp
	jmp *%ecx
L35:
	addl $36,%esp
	ret
