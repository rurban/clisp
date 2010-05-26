;; -*- Lisp -*- vim:filetype=lisp
;; some tests for GP/PARI CALCULATOR
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "pari/test")'

(require "pari") t
(listp (show (multiple-value-list (ext:module-info "pari" t)) :pretty t)) t

(format t "~&Version: ~S~%" pari:pari-version) NIL

(defparameter id (pari:identity-matrix 3)) ID
(pari:equal? id #Z"[1,0,0;0,1,0;0,0,1]") T
(pari:matrix-rank id) 3
(pari:equal? id (pari:matrix-transpose id)) T
(pari:equal? id (pari:adjoint-matrix id)) T
(pari:equal? (pari:matrix-kernel id) #Z"[;]") T
(pari:equal? id (pari:matrix-image id)) T
(pari:equal? id (pari:norm id)) T
(pari:equal? (pari:matrix-image-complement id) #Z"[]") T
(pari:equal? id (pari:matrix-supplement id)) T
(pari:equal? id (pari:matrix-eigenvectors id)) T
(pari:equal? id (pari:matrix-to-hessenberg-form id)) T
(pari:one? (pari:matrix-determinant id)) T
(pari:equal? (pari:l2-norm id) #Z"3") T
(pari:equal? (pari:pari-trace id) #Z"3") T
(pari:equal? (pari:pari-concatenate 1 2) #Z"[1,2]") T
(pari:equal? (pari:vector-extract id 1) #Z"[1;0;0]") T
;; (pari:equal? (pari:matrix-extract id 1 1) #Z"[1]") T
;; (pari:equal? (pari:matrix-extract id 1 2) #Z"[0]") T
(pari:equal? (pari:matrix-solve id #Z"[1;2;3]") #Z"[1;2;3]") T
(pari:equal? (pari:matrix-solve #Z"[1,1,1;0,1,1;0,0,1]" #Z"[1;2;3]")
             #Z"[-1;-1;3]") T
(pari:equal? (pari:characteristic-polynomial id) #Z"x^3-3*x^2+3*x-1") T
(pari:equal? id (pari:symmetric-matrix-sqred id)) T
(pari:equal? (pari:symmetric-matrix-signature id) #Z"[3,0]") T
(pari:equal? (pari:symmetric-matrix-sqred #Z"[2,1;1,2]") #Z"[2,1/2;0,3/2]") T
(pari:equal? (pari:matrix-indexrank #Z"[2,1;1,2]") #Z"[[1,2],[1,2]]") T
(pari:equal? (pari:symmetric-matrix-perfection id) #Z"3") T
(pari:pari-to-lisp (pari:matrix-determinant #2A((1 2) (3 4)))) -2
(pari:pari-to-lisp (pari:matrix-solve #2A((1 2) (3 4)) nil))
#2A((-2 1) (3/2 -1/2))

(pari:equal? (pari:pari-isqrt #Z"4") #Z"2") T
(pari:equal? (pari:pari-isqrt #Z"10") #Z"3") T
(pari:equal? (pari:factorial-integer 10) #Z"3628800") T

(pari:equal? (pari:best-rational-approximation (pari:pari-pi) #Z"100")
             #Z"22/7") T
(pari:equal? (pari:continued-fraction #Z"22/7") #Z"[3,7]") T
(pari:equal? (pari:best-rational-approximation (pari:pari-pi) #Z"10000")
             #Z"355/113") T
(pari:equal? (pari:continued-fraction #Z"355/113") #Z"[3,7,16]") T
(subseq (pari:pari-to-lisp (pari:continued-fraction (pari:pari-pi))) 0 6)
#(:ROW 3 7 15 1 292)

(pari:equal? (pari:fibonacci 10) #Z"55") T
(pari:pari-to-lisp (pari:fibonacci 100))  354224848179261915075
(pari:pari-to-lisp (pari:fibonacci 120))  5358359254990966640871840
(pari:equal? (pari:next-prime #Z"11") #Z"11") T
(pari:equal? (pari:next-prime #Z"15") #Z"17") T
(pari:equal? (pari:next-prime #Z"150") #Z"151") T
(pari:pari-to-lisp (pari:next-prime 1000)) 1009
(pari:pari-to-lisp (pari:next-prime 10000)) 10007
(pari:pari-to-lisp (pari:next-prime 100000)) 100003
(pari:pari-to-lisp (pari:next-prime (ash 1 70)))  1180591620717411303449
(pari:equal? (pari:nth-prime 100) #Z"541") T
(pari:equal? (pari:nth-prime 1000) #Z"7919") T
(pari:equal? (pari:nth-prime 10000) #Z"104729") T

(pari:equal? (pari:euler-phi #Z"6") #Z"2") T
(pari:equal? (pari:euler-phi #Z"13") #Z"12") T
(pari:pari-to-lisp (pari:euler-phi 28)) 12
(pari:pari-to-lisp (pari:euler-phi 130)) 48

(pari:equal? (pari:sum-divisors #Z"6") #Z"12") T
(pari:pari-to-lisp (pari:sum-divisors 28)) 56

(pari:equal? (pari:primitive-root #Z"7") #Z"Mod(3,7)") T
(pari:equal? (pari:primitive-root #Z"104729") #Z"Mod(12,104729)") T

(pari:equal? (pari:structure-of-z/n* #Z"7") #Z"[6, [6], [Mod(3, 7)]]") T
(pari:equal? (pari:structure-of-z/n* #Z"10") #Z"[4, [4], [Mod(7, 10)]]") T

(pari:pari-to-lisp (pari:divisors #Z"121")) #(:ROW 1 11 121)
(pari:pari-to-lisp (pari:divisors #Z"122")) #(:ROW 1 2 61 122)
(pari:pari-to-lisp (pari:divisors #Z"120"))
#(:ROW 1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120)
(pari:pari-to-lisp (pari:divisors #Z"144"))
#(:ROW 1 2 3 4 6 8 9 12 16 18 24 36 48 72 144)

(pari:pari-to-lisp #z"104")  104
(pari:pari-to-lisp #Z"[1,0,0;0,1,0;0,0,1]")
#2A((1 0 0) (0 1 0) (0 0 1))

(pari:pari-to-lisp (pari:pari-gcd 35 49))  7
(pari:pari-to-lisp (pari:pari-xgcd 35 49)) #(:ROW 3 -2 7)
(pari:pari-to-lisp (pari:pari-lcm 35 49))  245
