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
	jne L8
	movl (%esi),%eax
	call *%eax
	movl 8(%esi),%eax
	fstps (%eax)
	jmp L9
L8:
	cmpl $14,%eax
	jne L10
	movl (%esi),%eax
	call *%eax
	movl 8(%esi),%eax
	fstpl (%eax)
	jmp L9
L10:
	movl (%esi),%eax
	call *%eax
	movl %eax,%ebx
	movl 12(%esi),%eax
	cmpl $1,%eax
	je L9
	testl %eax,%eax
	je L61
	cmpl $2,%eax
	je L62
	cmpl $3,%eax
	je L62
	cmpl $4,%eax
	je L62
	cmpl $5,%eax
	je L63
	cmpl $6,%eax
	je L63
	cmpl $7,%eax
	je L61
	cmpl $8,%eax
	je L61
	cmpl $9,%eax
	je L61
	cmpl $10,%eax
	je L61
	movl 12(%esi),%ecx
	leal -11(%ecx),%eax
	cmpl $1,%eax
	jbe L64
	cmpl $15,%ecx
	je L61
	cmpl $16,%ecx
	jne L9
	movl 4(%esi),%eax
	testb $1,%al
	je L39
	movl 16(%esi),%eax
	cmpl $1,%eax
	jne L40
	movl 8(%esi),%edx
	movb (%ebx),%al
	movb %al,(%edx)
	jmp L9
L40:
	cmpl $2,%eax
	jne L42
	movl 8(%esi),%edx
	movw (%ebx),%ax
	movw %ax,(%edx)
	jmp L9
L42:
	cmpl $4,%eax
	jne L44
	movl 8(%esi),%edx
	movl (%ebx),%eax
	movl %eax,(%edx)
	jmp L9
L44:
	cmpl $8,%eax
	jne L46
	movl 8(%esi),%edx
	movl (%ebx),%eax
	movl %eax,(%edx)
	movl 8(%esi),%edx
	movl 4(%ebx),%eax
	movl %eax,4(%edx)
	jmp L9
L46:
	addl $3,%eax
	movl %eax,%ecx
	shrl $2,%ecx
	decl %ecx
	js L9
L50:
	movl 8(%esi),%edx
	movl (%ebx,%ecx,4),%eax
	movl %eax,(%edx,%ecx,4)
	decl %ecx
	jns L50
	jmp L9
L39:
	testb $2,%ah
	je L9
	movl 16(%esi),%eax
	cmpl $1,%eax
	jne L54
L62:
	movl 8(%esi),%eax
	movb %bl,(%eax)
	jmp L9
L54:
	cmpl $2,%eax
	jne L56
L63:
	movl 8(%esi),%eax
	movw %bx,(%eax)
	jmp L9
L56:
	cmpl $4,%eax
	jne L58
L61:
	movl 8(%esi),%eax
	movl %ebx,(%eax)
	jmp L9
L58:
	cmpl $8,%eax
	jne L9
L64:
	movl 8(%esi),%eax
	movl %ebx,(%eax)
	movl 8(%esi),%eax
	movl %edx,4(%eax)
L9:
	xorl %eax,%eax
	leal -8(%ebp),%esp
	popl %ebx
	popl %esi
	movl %ebp,%esp
	popl %ebp
	ret
