| GCC2 Library
| Jörg Höhle, 6-Jul-94
| GCC2 calls ___main() in _main()!

.text
	.even
.globl ___main
___main:
	rts
