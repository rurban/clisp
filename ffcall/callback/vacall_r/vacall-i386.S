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
	testl %edx,%edx
	je L3
	cmpl $1,%edx
	je L45
	cmpl $2,%edx
	jne L6
L45:
	movsbl 28(%esp),%eax
	jmp L3
L6:
	cmpl $3,%edx
	jne L8
	movzbl 28(%esp),%eax
	jmp L3
L8:
	cmpl $4,%edx
	jne L10
	movswl 28(%esp),%eax
	jmp L3
L10:
	cmpl $5,%edx
	jne L12
	movzwl 28(%esp),%eax
	jmp L3
L12:
	cmpl $6,%edx
	je L46
	cmpl $7,%edx
	je L46
	cmpl $8,%edx
	je L46
	cmpl $9,%edx
	je L46
	addl $-10,%edx
	cmpl $1,%edx
	ja L22
	movl 28(%esp),%eax
	movl 32(%esp),%edx
	jmp L3
L22:
	movl 16(%esp),%edx
	cmpl $12,%edx
	jne L24
	flds 28(%esp)
	jmp L3
L24:
	cmpl $13,%edx
	jne L26
	fldl 28(%esp)
	jmp L3
L26:
	cmpl $14,%edx
	jne L28
L46:
	movl 28(%esp),%eax
	jmp L3
L28:
	cmpl $15,%edx
	jne L3
	movl 4(%esp),%edx
	testb $1,%dl
	jne L47
	testb $4,%dh
	je L33
	movl 20(%esp),%edx
	cmpl $1,%edx
	jne L34
	movl 12(%esp),%edx
	movzbl (%edx),%eax
	jmp L3
L34:
	cmpl $2,%edx
	jne L37
	movl 12(%esp),%edx
	movzwl (%edx),%eax
	jmp L3
L37:
	cmpl $4,%edx
	jne L39
	movl 12(%esp),%edx
	movl (%edx),%eax
	jmp L3
L39:
	cmpl $8,%edx
	jne L33
	movl 12(%esp),%edx
	movl (%edx),%eax
	movl 4(%edx),%edx
	jmp L3
L33:
	movl 4(%esp),%edx
	testb $24,%dl
	jne L42
	addl $40,%esp
	ret $4
L42:
	testb $16,%dl
	je L3
L47:
	movl 12(%esp),%eax
L3:
	testb $2,5(%esp)
	je L44
	movl 40(%esp),%ecx
	movl 8(%esp),%esp
	jmp *%ecx
L44:
	popl %esi
	addl $36,%esp
	ret
