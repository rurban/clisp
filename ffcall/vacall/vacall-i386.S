#include "asmi386.h"
	.file	"vacall-i386.c"
gcc2_compiled.:
C(__gnu_compiled_c:)
.text
	.align 2
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
	testl %edx,%edx
	je L3
	cmpl $1,%edx
	je L45
	cmpl $2,%edx
	jne L6
L45:
	movsbl 24(%esp),%eax
	jmp L3
L6:
	cmpl $3,%edx
	jne L8
	movzbl 24(%esp),%eax
	jmp L3
L8:
	cmpl $4,%edx
	jne L10
	movswl 24(%esp),%eax
	jmp L3
L10:
	cmpl $5,%edx
	jne L12
	movzwl 24(%esp),%eax
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
	movl 24(%esp),%eax
	movl 28(%esp),%edx
	jmp L3
L22:
	movl 12(%esp),%edx
	cmpl $12,%edx
	jne L24
	flds 24(%esp)
	jmp L3
L24:
	cmpl $13,%edx
	jne L26
	fldl 24(%esp)
	jmp L3
L26:
	cmpl $14,%edx
	jne L28
L46:
	movl 24(%esp),%eax
	jmp L3
L28:
	cmpl $15,%edx
	jne L3
	movl (%esp),%edx
	testb $1,%dl
	jne L47
	testb $4,%dh
	je L33
	movl 16(%esp),%edx
	cmpl $1,%edx
	jne L34
	movl 8(%esp),%edx
	movzbl (%edx),%eax
	jmp L3
L34:
	cmpl $2,%edx
	jne L37
	movl 8(%esp),%edx
	movzwl (%edx),%eax
	jmp L3
L37:
	cmpl $4,%edx
	jne L39
	movl 8(%esp),%edx
	movl (%edx),%eax
	jmp L3
L39:
	cmpl $8,%edx
	jne L33
	movl 8(%esp),%edx
	movl (%edx),%eax
	movl 4(%edx),%edx
	jmp L3
L33:
	movl (%esp),%edx
	testb $24,%dl
	jne L42
	addl $36,%esp
	ret $4
L42:
	testb $16,%dl
	je L3
L47:
	movl 8(%esp),%eax
L3:
	testb $2,1(%esp)
	je L44
	movl 36(%esp),%ecx
	movl 4(%esp),%esp
	jmp *%ecx
L44:
	addl $36,%esp
	ret
