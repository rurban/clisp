#include "asmi386.h"
	.file	"vacall-i386.c"
gcc2_compiled.:
C(__gnu_compiled_c:)
.text
	.align 2
.globl C(__vacall_r)
C(__vacall_r:)
	subl $36,%esp
	pushl %esi
	movl $0,4(%esp)
	leal 44(%esp),%esi
	movl %esi,8(%esp)
	movl $0,12(%esp)
	movl $0,16(%esp)
	movl %ebx,36(%esp)
	leal 4(%esp),%edx
	pushl %edx
	movl 4(%ecx),%edx
	pushl %edx
	movl (%ecx),%edx
	call *%edx
	addl $8,%esp
	movl 16(%esp),%edx
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
	movsbl 28(%esp),%eax
	jmp L2
L6:
	movzbl 28(%esp),%eax
	jmp L2
L7:
	movswl 28(%esp),%eax
	jmp L2
L8:
	movzwl 28(%esp),%eax
	jmp L2
L14:
	movl 28(%esp),%eax
	movl 32(%esp),%edx
	jmp L2
L15:
	flds 28(%esp)
	jmp L2
L16:
	fldl 28(%esp)
	jmp L2
L17:
	movl 28(%esp),%eax
	jmp L2
L18:
	movl 4(%esp),%edx
	testb $1,%dl
	jne L36
	testb $4,%dh
	je L21
	movl 20(%esp),%edx
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
	movl 12(%esp),%edx
	movzbl (%edx),%eax
	jmp L2
L25:
	movl 12(%esp),%edx
	movzwl (%edx),%eax
	jmp L2
L26:
	movl 12(%esp),%edx
	movl (%edx),%eax
	jmp L2
L27:
	movl 12(%esp),%edx
	movl (%edx),%eax
	movl 4(%edx),%edx
	jmp L2
L21:
	movl 4(%esp),%edx
	testb $24,%dl
	jne L31
	addl $40,%esp
	ret $4
L31:
	testb $16,%dl
	je L2
L36:
	movl 12(%esp),%eax
L2:
	testb $2,5(%esp)
	je L35
	movl 40(%esp),%ecx
	movl 8(%esp),%esp
	jmp *%ecx
L35:
	popl %esi
	addl $36,%esp
	ret
