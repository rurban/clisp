#include "asmi386.h"
	.file	"avcall-i386.c"
gcc2_compiled.:
C(__gnu_compiled_c:)
.text
	.align 2
.globl C(__builtin_avcall)
C(__builtin_avcall:)
	pushl %ebp
	movl %esp,%ebp
	pushl %esi
	pushl %ebx
	movl 8(%ebp),%esi
	addl $-1024,%esp
	movl %esp,%ecx
	movl 20(%esi),%eax
	addl $-32,%eax
	subl %esi,%eax
	movl %eax,%edx
	sarl $2,%edx
	xorl %ebx,%ebx
	cmpl %edx,%ebx
	jge L3
L5:
	movl 32(%esi,%ebx,4),%eax
	movl %eax,(%ecx,%ebx,4)
	incl %ebx
	cmpl %edx,%ebx
	jl L5
L3:
	testb $8,4(%esi)
	je L7
	cmpl $16,12(%esi)
	jne L7
#APP
	movl 8(%esi),%ebx
#NO_APP
L7:
	movl 12(%esi),%eax
	cmpl $13,%eax
	je L9
	cmpl $14,%eax
	je L10
	jmp L11
L9:
	movl (%esi),%eax
	call *%eax
	movl 8(%esi),%eax
	fstps (%eax)
	jmp L13
L10:
	movl (%esi),%eax
	call *%eax
	movl 8(%esi),%eax
	fstpl (%eax)
	jmp L13
L11:
	movl (%esi),%eax
	call *%eax
	movl %eax,%ebx
	movl 12(%esi),%eax
	cmpl $16,%eax
	ja L13
	jmp *L55(,%eax,4)
L55:
	.long L49
	.long L13
	.long L47
	.long L47
	.long L47
	.long L48
	.long L48
	.long L49
	.long L49
	.long L49
	.long L49
	.long L50
	.long L50
	.long L13
	.long L13
	.long L49
	.long L30
L30:
	movl 4(%esi),%eax
	testb $1,%al
	je L31
	movl 16(%esi),%eax
	cmpl $2,%eax
	je L34
	ja L43
	cmpl $1,%eax
	je L33
	jmp L37
L43:
	cmpl $4,%eax
	je L35
	cmpl $8,%eax
	je L36
	jmp L37
L33:
	movl 8(%esi),%edx
	movb (%ebx),%al
	movb %al,(%edx)
	jmp L13
L34:
	movl 8(%esi),%edx
	movw (%ebx),%ax
	movw %ax,(%edx)
	jmp L13
L35:
	movl 8(%esi),%edx
	movl (%ebx),%eax
	movl %eax,(%edx)
	jmp L13
L36:
	movl 8(%esi),%edx
	movl (%ebx),%eax
	movl %eax,(%edx)
	movl 8(%esi),%edx
	movl 4(%ebx),%eax
	movl %eax,4(%edx)
	jmp L13
L37:
	movl 16(%esi),%eax
	addl $3,%eax
	movl %eax,%ecx
	shrl $2,%ecx
	decl %ecx
	js L13
L40:
	movl 8(%esi),%edx
	movl (%ebx,%ecx,4),%eax
	movl %eax,(%edx,%ecx,4)
	decl %ecx
	jns L40
	jmp L13
L31:
	testb $1,%ah
	je L13
	movl 16(%esi),%eax
	cmpl $2,%eax
	je L48
	ja L53
	cmpl $1,%eax
	je L47
	jmp L13
L53:
	cmpl $4,%eax
	je L49
	cmpl $8,%eax
	je L50
	jmp L13
L47:
	movl 8(%esi),%eax
	movb %bl,(%eax)
	jmp L13
L48:
	movl 8(%esi),%eax
	movw %bx,(%eax)
	jmp L13
L49:
	movl 8(%esi),%eax
	movl %ebx,(%eax)
	jmp L13
L50:
	movl 8(%esi),%eax
	movl %ebx,(%eax)
	movl 8(%esi),%eax
	movl %edx,4(%eax)
L13:
	xorl %eax,%eax
	leal -8(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
