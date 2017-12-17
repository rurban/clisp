;; -*- Lisp -*- vim:filetype=lisp
;; some tests for GP/PARI CALCULATOR
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "../modules/pari/test" :logname "pari/test")'

(list (null (require "pari"))) (#-PARI NIL #+PARI T)
(listp (show (multiple-value-list (ext:module-info "pari" t)) :pretty t)) t

(format t "~&Version: ~S~%" pari:pari-version) NIL

(without-package-lock ("PARI")
  (defparameter *pari-to-lisp* (make-hash-table))
  (defparameter *lisp-to-pari* (make-hash-table))
  (setq *trace-output* (make-broadcast-stream)) ; suppress TRACE output
  (trace (pari::convert-from-pari
          :pre (incf (gethash (pari::pari-type-raw (car ext:*trace-args*))
                              *pari-to-lisp* 0))))
  ;; this is not good enough because it captures only the top-level calls to
  ;; `pari-to-lisp', not the internal calls to `convert-from-pari'.
  ;; (defmethod pari::pari-to-lisp :before ((x pari::pari-object-internal))
  ;;   (incf (gethash (pari::pari-type-raw (pari::pari-object-internal-pointer
  ;;                                        x)) *pari-to-lisp* 0)))
  (defmethod pari::convert-to-pari :around ((x t))
    (let ((ptr (call-next-method)))
      (when ptr (incf (gethash (pari::pari-type-raw ptr) *lisp-to-pari* 0)))
      ptr))
  t) T

(defparameter ltop (show pari:pari-avma)) ltop

;; integer conversion
(defun roundtrip1 (x)
  (pari:pari-to-lisp (read-from-string (format nil "#Z\"~D\"" x))))
ROUNDTRIP1
(defun roundtrip2 (x)
  (read-from-string (pari::%write-to-string (pari::convert-to-pari x))))
ROUNDTRIP2
(defun get-x-ash (i) (random (ash 1 i))) GET-X-ASH
(defun get-x-ash-neg (i) (- (random (ash 1 i)))) GET-X-ASH-NEG
(defun check-roundtrip (limit get-x roundtrip)
  (loop :for i :from 1 :to limit :for x = (funcall get-x i)
    :for px = (funcall roundtrip x)
    :unless (= x px) :collect (cons x px)))
CHECK-ROUNDTRIP
(check-roundtrip 1000 #'get-x-ash #'roundtrip1) ()
(check-roundtrip 1000 #'get-x-ash-neg #'roundtrip1) ()
(check-roundtrip 1000 #'get-x-ash #'roundtrip2) ()
(check-roundtrip 1000 #'get-x-ash-neg #'roundtrip2) ()

;; generic conversion
(defun roundtrip-1 (x)
  (pari:pari-to-lisp
   (read-from-string
    (format nil "#Z\"~A\"" (pari::%write-to-string
                            (pari::convert-to-pari x))))))
ROUNDTRIP-1
(roundtrip-1 #(1 2 3)) #(1 2 3)
(roundtrip-1 #(:row 1 2 3)) #(:row 1 2 3)
(roundtrip-1 #(:col 1 2 3)) #(:col 1 2 3)
(defun roundtrip-2 (x)
  (read-from-string
   (format nil "#Z\"~A\"" (pari::%write-to-string
                           (pari::convert-to-pari
                            (pari:pari-to-lisp x))))))
ROUNDTRIP-2
(pari:gequal (roundtrip-2 #Z"Vecsmall([1,2])") #Z"Vecsmall([1,2])") T
(pari:gequal (roundtrip-2 #Z"[1,2]") #Z"[1,2]") T
(pari:gequal (roundtrip-2 #Z"[1;2]") #Z"[1;2]") T

(defparameter mycol #Z"[1;2;3]") mycol
(pari:sizeword mycol) 15
(pari:sizebyte mycol) 120
(pari::%write-to-string-pretty (pari::convert-to-pari mycol))
"
[1]

[2]

[3]
"
(pari::%write-to-string-TeX (pari::convert-to-pari mycol))
"\\pmatrix{
 1\\cr
 2\\cr
 3\\cr
 }"

(defparameter myrow #Z"[1,2,3]") myrow
(pari:sizeword myrow) 13
(pari:sizebyte myrow) 104
(pari::%write-to-string-pretty (pari::convert-to-pari myrow))
"[1, 2, 3]"
(pari::%write-to-string-TeX (pari::convert-to-pari myrow))
"\\pmatrix{ 1&2&3\\cr}
"

(defparameter id (pari:matid 3)) ID
(pari:gequal id #Z"[1,0,0;0,1,0;0,0,1]") T
(pari:gequal id (pari:matdiagonal #Z"[1,1,1]")) T
(pari:matrank id) 3
(pari:gequal id (pari:mattranspose id)) T
(pari:gequal id (pari:matadjoint id)) T
(pari:gequal (pari:matker id) #Z"[;]") T
(pari:gequal id (pari:matimage id)) T
(pari:gequal id (pari:norm id)) T
(pari:gequal (pari:matimagecompl id) #Z"Vecsmall([])") T
(pari:gequal id (pari:matsupplement id)) T
(pari:gequal id (pari:mateigen id)) T
(pari:gequal id (pari:mathess id)) T
(pari:pari-to-lisp (pari:matdet id)) 1
(pari:pari-to-lisp (pari:norml2 id)) 3
(pari:pari-to-lisp (pari:trace_ id)) 3
(pari:gequal (pari:concat 1 :y 2) #Z"[1,2]") T
(pari:gequal (pari:vecextract id 1) #Z"[1;0;0]") T
(pari:gequal (pari:vecextract id 7 :z 7) id) T
(pari:gequal (pari:vecextract id #(:row 1 2 3) :z #(:row 1 2 3)) id) T
(pari:gequal (pari:matsolve id #Z"[1;2;3]") #Z"[1;2;3]") T
(pari:gequal (pari:matsolve #Z"[1,1,1;0,1,1;0,0,1]" #Z"[1;2;3]")
             #Z"[-1;-1;3]") T
(pari:gequal (pari:charpoly id) #Z"x^3-3*x^2+3*x-1") T
(pari:gequal id (pari:qfgaussred id)) T
(pari:gequal (pari:qfsign id) #Z"[3,0]") T
(pari:gequal (pari:qfgaussred #Z"[2,1;1,2]") #Z"[2,1/2;0,3/2]") T
(pari:gequal (pari:matindexrank #Z"[2,1;1,2]")
             #Z"[Vecsmall([1, 2]), Vecsmall([1, 2])]") T
(pari:gequal (pari:qfperfection id) #Z"3") T
(pari:pari-to-lisp (pari:matdet #2A((1 2) (3 4)))) -2
(pari:pari-to-lisp (pari:matsolve #2A((1 2) (3 4)) nil))
#2A((-2 1) (3/2 -1/2))
(pari:pari-to-lisp (pari:_*_ #2A((1 2) (3 4)) #(:COL 1 2))) #(:COL 5 11)
(pari:pari-to-lisp (pari:matsolve #2A((1 2) (3 4)) #(:COL 5 11)))#(:COL 1 2)
(pari:pari-to-lisp (pari:sqr #2A((2 1) (1 2)))) #2A((5 4) (4 5))
(pari:pari-to-lisp (pari:_/_ #2A((5 4) (4 5)) #2A((2 1) (1 2))))
#2A((2 1) (1 2))

(pari:pari-to-lisp (pari:matindexrank id))
#(:ROW #(1 2 3) #(1 2 3))
(pari:pari-to-lisp (pari:matindexrank #2A((1 2) (2 4))))
#(:ROW #(1) #(1))
(let* ((mx #2A((1 2 3) (2 4 6) (1 2 8)))
       (ir (pari:pari-to-lisp (pari:matindexrank mx))))
  (list ir (pari:pari-to-lisp (pari:vecextract
                               mx (aref ir 1) :z (aref ir 2)))))
(#(:ROW #(1 3) #(1 3)) #2A((1 3) (1 8)))

(pari:gequal (pari:matdiagonal (pari:scalarcol #Z"x^2" 1))
             (pari:scalarmat #Z"x^2" 1)) T

(pari:pari-to-lisp (pari:sqr (pari:sqrt_ #C(0 1))))
#C(0 0.99999999999999999995L0)
(pari:pari-to-lisp (pari:sqrt_ #C(1 2)))
#C(1.2720196495140689642L0 0.78615137775742328604L0)
(pari:pari-to-lisp (pari:sqrt_ #C(1 1)))
#C(1.0986841134678099661L0 0.45508986056222734128L0)
(pari:pari-to-lisp (pari:sqrt_ #C(0 1)))
#C(0.7071067811865475244L0 0.7071067811865475244L0)

(pari:gequal (pari:sqrtint #Z"4") #Z"2") T
(pari:gequal (pari:sqrtint #Z"10") #Z"3") T
(pari:gequal (pari:_! 10) #Z"3628800") T
(pari:pari-to-lisp (pari:gamma 10)) 362880.0L0
(pari:pari-to-lisp (pari:gammah 9.5)) 362880.0L0

(pari:gequal (pari:-_ (pari:I)) (pari:conj (pari:I))) T

(pari:gequal (pari:bestappr (pari:Pi) :|b| #Z"100") #Z"22/7") T
(pari:gequal (pari:contfrac #Z"22/7") #Z"[3,7]") T
(pari:gequal (pari:bestappr (pari:Pi) :|b| #Z"10000") #Z"355/113") T
(pari:gequal (pari:contfrac #Z"355/113") #Z"[3,7,16]") T
(pari:pari-to-lisp (pari:contfrac (pari:Pi)))
#(:ROW 3 7 15 1 292 1 1 1 2 1 3 1 14 2 1 1 2 2)
(pari:pari-to-lisp (pari:contfrac (pari:Euler)))
#(:ROW 0 1 1 2 1 2 1 4 3 13 5 1 1 8 1 2 4 1 1 41)
(pari:pari-to-lisp (pari:contfrac (exp 1l0)))
#(:ROW 2 1 2 1 1 4 1 1 6 1 1 8 1 1 10 1 1 12 1 1 14 2)
(defparameter picf (pari:contfrac (pari:Pi :prec 30) :nmax 30)) PICF
(pari:pari-to-lisp picf)
#(:ROW 3 7 15 1 292 1 1 1 2 1 3 1 14 2 1 1 2 2 2 2 1 84 2 1 1 15 3 13 1 4)
(pari:pari-to-lisp (pari:contfracpnqn picf :n 3))
#2A((3 22 333 355) (1 7 106 113))
(pari:pari-to-lisp (pari:contfracpnqn picf :n 7))
#2A((3 22 333 355 103993 104348 208341 312689)
    (1 7 106 113 33102 33215 66317 99532))

(pari:gequal (pari:fibonacci 10) #Z"55") T
(pari:pari-to-lisp (pari:fibonacci 100))  354224848179261915075
(pari:pari-to-lisp (pari:fibonacci 120))  5358359254990966640871840
(pari:gequal (pari:nextprime #Z"11") #Z"11") T
(pari:gequal (pari:nextprime #Z"15") #Z"17") T
(pari:gequal (pari:nextprime #Z"150") #Z"151") T
(pari:pari-to-lisp (pari:nextprime 1000)) 1009
(pari:pari-to-lisp (pari:nextprime 10000)) 10007
(pari:pari-to-lisp (pari:nextprime 100000)) 100003
(pari:pari-to-lisp (pari:nextprime (ash 1 70)))  1180591620717411303449
(pari:gequal (pari:prime 100) #Z"541") T
(pari:gequal (pari:prime 1000) #Z"7919") T
(pari:gequal (pari:prime 10000) #Z"104729") T

(pari:gequal (pari:eulerphi #Z"6") #Z"2") T
(pari:gequal (pari:eulerphi #Z"13") #Z"12") T
(pari:pari-to-lisp (pari:eulerphi 28)) 12
(pari:pari-to-lisp (pari:eulerphi 130)) 48

(pari:gequal (pari:sigma #Z"6") #Z"12") T
(pari:pari-to-lisp (pari:sigma 28)) 56
(pari:pari-to-lisp (pari:factor #Z"120"))  #2A((2 3) (3 1) (5 1))
(pari:pari-to-lisp (pari:factor #Z"144"))  #2A((2 4) (3 2))
(pari:gequal1 (pari:isprime 139)) T
(pari:gequal0 (pari:ispseudoprime 140)) T
(pari:bigomega 12) 3
(pari:omega 12) 2
(pari:bigomega 144) 6
(pari:omega 144) 2

(pari:issquarefree 0) 0
(pari:issquarefree 1) 1
(pari:issquarefree 9) 0
(pari:issquarefree 10) 1
(pari:issquarefree #Z"x^5") 0
(pari:issquarefree #Z"x^2+1") 1
(pari:issquarefree #Z"x^3+2*x^2+x") 0

(multiple-value-bind (b r) (pari:issquare 0)
  (list b (pari:pari-to-lisp r))) (1 0)
(multiple-value-bind (b r) (pari:issquare 1)
  (list b (pari:pari-to-lisp r))) (1 1)
(multiple-value-bind (b r) (pari:issquare 4)
  (list b (pari:pari-to-lisp r))) (1 2)
(multiple-value-list (pari:issquare 10)) (0 NIL)
(multiple-value-bind (b r) (pari:issquare #Z"x^2")
  (list b (pari:pari-to-lisp r)))
(1 #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(0 1)))
(multiple-value-list (pari:issquare #Z"x^2+1")) (0 NIL)
(multiple-value-bind (b r) (pari:issquare #Z"x^2+2*x+1")
  (list b (pari:pari-to-lisp r)))
(1 #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 1)))

(pari:pari-to-lisp (pari:stirling 5 2)) -50
(pari:pari-to-lisp (pari:stirling 5 3 :flag 1)) 35
(pari:pari-to-lisp (pari:stirling 10 6 :flag 2)) 22827

(pari:pari-to-lisp (pari:moebius 2)) -1
(pari:pari-to-lisp (pari:moebius 6)) 1
(pari:pari-to-lisp (pari:moebius 16)) 0

(pari:pari-to-lisp (pari:polresultantext 40 60)) #(:ROW 1/40 0 1)

(pari:gequal (pari:znprimroot #Z"7") #Z"Mod(3,7)") T
(pari:gequal (pari:znprimroot #Z"104729") #Z"Mod(12,104729)") T
(pari:pari-to-lisp (pari:znprimroot 100003))
#S(PARI:pari-integermod :MODULUS 100003 :REP 2)
(pari:pari-to-lisp (pari:znprimroot 1180591620717411303449))
#S(PARI:pari-integermod :MODULUS 1180591620717411303449 :REP 3)
(pari:pari-to-lisp (pari:znprimroot 3))
#S(PARI:pari-integermod :MODULUS 3 :REP 2)
(pari:pari-to-lisp (pari:znprimroot 7))
#S(PARI:pari-integermod :MODULUS 7 :REP 3)

(loop :repeat 5000 :with max
  :for prime = 2 :then (pari:pari-to-lisp (pari:nextprime (1+ prime)))
  :for znprimroot = (pari:pari-to-lisp (pari:znprimroot prime))
  :for primitive = (pari:pari-integermod-rep znprimroot)
  :do (assert (= prime (pari:pari-integermod-modulus znprimroot)))
  (unless (and max (>= (pari:pari-integermod-rep (car max)) primitive))
    (push znprimroot max))
  :finally (return (nreverse max)))
(#S(PARI:pari-integermod :MODULUS 2 :REP 1)
 #S(PARI:pari-integermod :MODULUS 3 :REP 2)
 #S(PARI:pari-integermod :MODULUS 7 :REP 3)
 #S(PARI:pari-integermod :MODULUS 23 :REP 5)
 #S(PARI:pari-integermod :MODULUS 41 :REP 6)
 #S(PARI:pari-integermod :MODULUS 71 :REP 7)
 #S(PARI:pari-integermod :MODULUS 191 :REP 19)
 #S(PARI:pari-integermod :MODULUS 409 :REP 21)
 #S(PARI:pari-integermod :MODULUS 2161 :REP 23)
 #S(PARI:pari-integermod :MODULUS 5881 :REP 31)
 #S(PARI:pari-integermod :MODULUS 36721 :REP 37))

(pari:gequal (pari:znstar #Z"7") #Z"[6, [6], [Mod(3, 7)]]") T
(pari:gequal (pari:znstar #Z"10") #Z"[4, [4], [Mod(7, 10)]]") T
(pari:pari-to-lisp (pari:znstar 12))
#(:ROW 4 #(:ROW 2 2)
  #(:ROW #S(PARI:pari-integermod :MODULUS 12 :REP 7)
    #S(PARI:pari-integermod :MODULUS 12 :REP 5)))
(pari:pari-to-lisp (pari:znstar 24))
#(:ROW 8 #(:ROW 2 2 2)
  #(:ROW #S(PARI:pari-integermod :MODULUS 24 :REP 7)
    #S(PARI:pari-integermod :MODULUS 24 :REP 13)
    #S(PARI:pari-integermod :MODULUS 24 :REP 17)))

(pari:pari-to-lisp (pari:chinese
                    #S(PARI:pari-integermod :MODULUS 10 :REP 7)
                    :y #S(PARI:pari-integermod :MODULUS 12 :REP 5)))
#S(PARI:pari-integermod :MODULUS 60 :REP 17)
(pari:pari-to-lisp (pari:chinese
                    #S(PARI:pari-integermod :MODULUS 10 :REP 7)
                    :y #S(PARI:pari-integermod :MODULUS 3 :REP 4)))
#S(PARI:pari-integermod :MODULUS 30 :REP 7)

(pari:pari-to-lisp (pari:poldisc #Z"x^2-1")) 4
(pari:pari-to-lisp (pari:polresultant #Z"x^2-1" #Z"x^3")) -1

(pari:pari-to-lisp (pari:divisors #Z"121")) #(:ROW 1 11 121)
(pari:pari-to-lisp (pari:divisors #Z"122")) #(:ROW 1 2 61 122)
(pari:pari-to-lisp (pari:divisors #Z"120"))
#(:ROW 1 2 3 4 5 6 8 10 12 15 20 24 30 40 60 120)
(pari:pari-to-lisp (pari:divisors #Z"144"))
#(:ROW 1 2 3 4 6 8 9 12 16 18 24 36 48 72 144)

(pari:pari-to-lisp #Z"104")  104
(pari:pari-to-lisp #Z"[1,0,0;0,1,0;0,0,1]")
#2A((1 0 0) (0 1 0) (0 0 1))

(pari:pari-to-lisp (pari:gcd_ 35 :y 49))  7
(pari:pari-to-lisp (pari:gcdext 35 49)) #(:ROW 3 -2 7)
(pari:pari-to-lisp (pari:lcm_ 35 :y 49))  245

(pari:pari-to-lisp (pari:binomial 3 1)) 3
(pari:pari-to-lisp (pari:binomial 30 10)) 30045015
(pari:pari-to-lisp (pari:binomial 300 10)) 1398320233241701770

(pari:gequal (pari:pollegendre 0) #Z"1") T
(pari:gequal (pari:pollegendre 1) #Z"x") T
(pari:gequal (pari:pollegendre 2) #Z"3/2*x^2 - 1/2") T
(pari:gequal (pari:pollegendre 3) #Z"5/2*x^3 - 3/2*x") T
(pari:gequal (pari:pollegendre 4) #Z"35/8*x^4 - 15/4*x^2 + 3/8") T
(pari:gequal (pari:pollegendre 5) #Z"63/8*x^5 - 35/4*x^3 + 15/8*x") T
(let ((legendre6 (pari:pollegendre 6)))
  (multiple-value-bind (primpart content)
      (pari:primitive-part legendre6)
    (list (pari:gequal (pari:_*_ primpart content) legendre6)
          (pari:gequal primpart (pari:primpart legendre6))
          (pari:pari-to-lisp content))))
(T T 1/16)
(pari:pari-to-lisp (pari:content (pari:pollegendre 7)))  1/16
(pari:pari-to-lisp (pari:content (pari:pollegendre 8)))  1/128
(pari:pari-to-lisp (pari:content (pari:pollegendre 9)))  1/128
(pari:pari-to-lisp (pari:content (pari:pollegendre 10)))  1/256

(pari:gequal (pari:polchebyshev 0) #Z"1") T
(pari:gequal (pari:polchebyshev 1) #Z"x") T
(pari:gequal (pari:polchebyshev 2) #Z"2*x^2 - 1") T
(pari:gequal (pari:polchebyshev 3) #Z"4*x^3 - 3*x") T
(pari:gequal (pari:polchebyshev 4) #Z"8*x^4 - 8*x^2 + 1") T
(pari:gequal (pari:polchebyshev 5) #Z"16*x^5 - 20*x^3 + 5*x") T
(let ((chebyshev6 (pari:polchebyshev 6)))
  (multiple-value-bind (primpart content) (pari:primitive-part chebyshev6)
    (list (pari:gequal primpart chebyshev6)
          (pari:gequal primpart (pari:primpart chebyshev6))
          (pari:pari-to-lisp content))))
(T T NIL)                       ; NIL means 1
(pari:pari-to-lisp (pari:content (pari:polchebyshev 7)))  1
(pari:pari-to-lisp (pari:content (pari:polchebyshev 8)))  1
(pari:pari-to-lisp (pari:content (pari:polchebyshev 9)))  1
(pari:pari-to-lisp (pari:content (pari:polchebyshev 10))) 1
(pari:polisirreducible (pari:polchebyshev 8))  1
(pari:polisirreducible (pari:polchebyshev 16)) 1
(pari:polisirreducible (pari:polchebyshev 32)) 1
(pari:polisirreducible (pari:polchebyshev 64)) 1

(pari:pari-to-lisp (pari:factor #z"x^3"))
#2A((#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(0 1)) 3))
(pari:pari-to-lisp (pari:factor (pari:polchebyshev 11)))
#2A((#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(0 1)) 1)
    (#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS
        #(-11 0 220 0 -1232 0 2816 0 -2816 0 1024)) 1))

(pari:pari-to-lisp (pari:mathilbert 4))
#2A((1 1/2 1/3 1/4) (1/2 1/3 1/4 1/5) (1/3 1/4 1/5 1/6) (1/4 1/5 1/6 1/7))
(pari:pari-to-lisp (pari:mathilbert 7))
#2A((1 1/2 1/3 1/4 1/5 1/6 1/7)
    (1/2 1/3 1/4 1/5 1/6 1/7 1/8)
    (1/3 1/4 1/5 1/6 1/7 1/8 1/9)
    (1/4 1/5 1/6 1/7 1/8 1/9 1/10)
    (1/5 1/6 1/7 1/8 1/9 1/10 1/11)
    (1/6 1/7 1/8 1/9 1/10 1/11 1/12)
    (1/7 1/8 1/9 1/10 1/11 1/12 1/13))

(pari:pari-to-lisp (pari:matpascal 10))
#2A((1 0 0 0 0 0 0 0 0 0 0)
    (1 1 0 0 0 0 0 0 0 0 0)
    (1 2 1 0 0 0 0 0 0 0 0)
    (1 3 3 1 0 0 0 0 0 0 0)
    (1 4 6 4 1 0 0 0 0 0 0)
    (1 5 10 10 5 1 0 0 0 0 0)
    (1 6 15 20 15 6 1 0 0 0 0)
    (1 7 21 35 35 21 7 1 0 0 0)
    (1 8 28 56 70 56 28 8 1 0 0)
    (1 9 36 84 126 126 84 36 9 1 0)
    (1 10 45 120 210 252 210 120 45 10 1))
(pari:pari-to-lisp (pari:matpascal 10 :q 2))
#2A((1 0 0 0 0 0 0 0 0 0 0)
    (1 1 0 0 0 0 0 0 0 0 0)
    (1 3 1 0 0 0 0 0 0 0 0)
    (1 7 7 1 0 0 0 0 0 0 0)
    (1 15 35 15 1 0 0 0 0 0 0)
    (1 31 155 155 31 1 0 0 0 0 0)
    (1 63 651 1395 651 63 1 0 0 0 0)
    (1 127 2667 11811 11811 2667 127 1 0 0 0)
    (1 255 10795 97155 200787 97155 10795 255 1 0 0)
    (1 511 43435 788035 3309747 3309747 788035 43435 511 1 0)
    (1 1023 174251 6347715 53743987 109221651 53743987 6347715 174251 1023 1))
(pari:pari-to-lisp (pari:matpascal 7 :q 3))
#2A((1 0 0 0 0 0 0 0)
    (1 1 0 0 0 0 0 0)
    (1 4 1 0 0 0 0 0)
    (1 13 13 1 0 0 0 0)
    (1 40 130 40 1 0 0 0)
    (1 121 1210 1210 121 1 0 0)
    (1 364 11011 33880 11011 364 1 0)
    (1 1093 99463 925771 925771 99463 1093 1))

(pari:pari-to-lisp (pari:numtoperm 10 3628795)) #(:ROW 10 9 8 7 6 5 4 1 3 2)
(pari:pari-to-lisp (pari:permtonum #(:ROW 10 9 8 7 6 5 4 1 3 2))) 3628795
(pari:pari-to-lisp (pari:numtoperm 9 362831)) #(:ROW 9 8 7 6 3 5 4 2 1)
(pari:pari-to-lisp (pari:permtonum #(:ROW 9 8 7 6 3 5 4 2 1))) 362831

(pari:isexactzero 0) T
(pari:isexactzero 0.0) NIL
(pari:isexactzero 1) NIL
(pari:isrationalzero 0) T
(pari:isrationalzero 0.0) NIL
(pari:isrationalzero #(:COL)) T
(pari:isrationalzero #(:ROW)) T
(pari:isrationalzero #(:COL 1)) NIL
(pari:isrationalzero #(:ROW 2.0)) NIL
(pari:isrationalzeroscalar 0) T
(pari:isrationalzeroscalar 0.0) NIL
(pari:isrationalzeroscalar #(:COL)) NIL
(pari:isrationalzeroscalar #(:ROW)) NIL
(pari:isrationalzeroscalar #(:COL 1)) NIL
(pari:isrationalzeroscalar #(:ROW 2.0)) NIL

(pari:bigint? #Z"0") NIL
(pari:bigint? #Z"1") NIL
(pari:bigint? #Z"10") NIL

(let ((seed (pari:pari-to-lisp (pari:getrand))))
  (show (integer-length seed))
  (list (multiple-value-list (pari:setrand 100))
        (pari:bigint? seed)
        (and (= seed (pari:pari-to-lisp (pari:getrand)))
             seed)))
(NIL T NIL)
(<= 0 (pari:pari-to-lisp (pari:random_ :|n| 10)) 9) T
(< 0 (pari:pari-to-lisp (pari:random_ :|n| 1.0)) 1) T
(pari:pari-type (show (pari:random_))) PARI:INT

(integerp (show (pari:getstack))) T
(integerp (show (pari:gettime))) T
(svref (show (pari:pari-to-lisp (pari:getheap))) 0) :ROW

(integerp (show (pari:maxprime))) T

(pari:pari-to-lisp (pari:polchebyshev 4))
#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 0 -8 0 8))

(pari:pari-to-lisp (pari:polchebyshev 40))
#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS
   #(1 0 -800 0 106400 0 -5617920 0 156900480 0 -2677768192 0 30429184000
     0 -243433472000 0 1424085811200 0 -6254808268800 0 21002987765760 0
     -54553214976000 0 110292369408000 0 -173752901959680 0 212364657950720
     0 -199183403319296 0 140552804761600 0 -72155450572800 0 25426206392320
     0 -5497558138880 0 549755813888))

(pari:pari-to-lisp (pari:polroots #z"x^2+1"))
#(:COL #C(0 -1.0L0) #C(0 1.0L0))
(pari:pari-to-lisp (pari:polsym #z"x^2+1" 4))
#(:COL 2 0 -2 0 2)
(pari:pari-to-lisp (pari:polsym (pari:polchebyshev 30) 30))
#(:COL 30 0 15 0 45/4 0 75/8 0 525/64 0 945/128 0 3465/512 0 6435/1024 0
  96525/16384 0 182325/32768 0 692835/131072 0 1322685/262144 0
  10140585/2097152 0 19501125/4194304 0 75218625/16777216 0 145422675/33554432)
(pari:pari-to-lisp (pari:polsym (pari:pollegendre 12) 12))
#(:COL 12 0 132/23 0 15444/3703 0 5477076/1618211 0 12809498724/4429043507 0
  259405490124/101868000661 0 9262490692392036/4050984782285987)

(let ((order (pari:variables)))
  (list (pari:pari-to-lisp order) (princ-to-string order)))
(#(:ROW #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(0 1))
   #S(PARI:pari-poly :S 1 :VARNO 1 :COEFFS #(0 1)))
  "#Z\"[x, y]\"")
(pari:pari-to-lisp (pari:variables :x #Z"z^7"))
#(:ROW #S(PARI:pari-poly :S 1 :VARNO 10 :COEFFS #(0 1)))
(pari:pari-to-lisp (pari:variables :x #Z"z^7+y"))
#(:ROW #S(PARI:pari-poly :S 1 :VARNO 1 :COEFFS #(0 1))
  #S(PARI:pari-poly :S 1 :VARNO 10 :COEFFS #(0 1)))
(pari:pari-to-lisp (pari:variables :x nil))
#(:ROW #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(0 1))
   #S(PARI:pari-poly :S 1 :VARNO 1 :COEFFS #(0 1))
  #S(PARI:pari-poly :S 1 :VARNO 10 :COEFFS #(0 1)))

(pari:gequal (pari:variable-numbers #Z"x") #Z"Vecsmall([0])") T
(pari:pari-to-lisp (pari:variable-numbers #Z"y^2")) #(1)

(loop for i from 0 to 20 collect (pari:pari-to-lisp (pari:bernfrac i)))
(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6
 0 -3617/510 0 43867/798 0 -174611/330)

(pari:sign 2) 1
(pari:sign -10) -1
(pari:sign 0) 0
(pari:sign 0l0) 0
(pari:sign 1f0) 1
(pari:sign -1d0) -1

(pari:pari-to-lisp (pari:vecsort #(:ROW 10 40 30 20))) #(:ROW 10 20 30 40)
(pari:pari-to-lisp (pari:vecsort #(:ROW 10 40 30 20) :flag 1)) #(1 4 3 2)
(pari:pari-to-lisp (pari:vecsort
                    #(:ROW #(:ROW 1 10) #(:ROW 2 40) #(:ROW 3 30) #(:ROW 4 20))
                    :cmpf 2))
#(:ROW #(:ROW 1 10) #(:ROW 4 20) #(:ROW 3 30) #(:ROW 2 40))
(pari:pari-to-lisp (pari:vecsort #(:COL 1 2 3 4 3 2 1) :flag (logior 4 8)))
#(:COL 4 3 2 1)
(pari:pari-to-lisp (pari:vecsort
                    #(:ROW #(:ROW 1 10 #(0)) #(:ROW 2 40 #(1))
                      #(:ROW 3 20 #(2)) #(:ROW 0 20 #(3)))
                    :cmpf #(2 1) :flag 8))
#(:ROW #(:ROW 1 10 #(0)) #(:ROW 0 20 #(3)) #(:ROW 3 20 #(2)) #(:ROW 2 40 #(1)))

pari:pari-real-precision  19
(length (prin1-to-string (pari:pari-to-lisp (pari:Pi))))  23
(length (prin1-to-string (pari:pari-to-lisp (pari:Pi :prec 38)))) 41
(loop :for n :from 3 :to 50
  :unless (= (- n 2) (length (pari::pari-mantissa (pari::%Pi n))))
  :collect n)
()
(loop :for n :from 3 :to 50
  :for mypi0 = (pari:pari-to-lisp (pari:Pi :prec n))
  :for mypi1 = (pari::convert-from-pari (pari::convert-to-pari mypi0))
  :unless (= mypi0 mypi1) :collect (list n mypi0 mypi1))
()
(= pi (pari:pari-to-lisp (pari:Pi))) T

(pari:pari-to-lisp #Z"(1-x)^2/(1+x)^2")
#S(PARI:pari-ratfun :NUMER #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 -2 1))
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 2 1)))
(pari:pari-to-lisp (pari:|_'| #Z"(1-x)^2/(1+x)^2"))
#S(PARI:pari-ratfun :NUMER #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-4 4))
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 3 3 1)))
(pari:pari-to-lisp (pari:|_'| #Z"1/(1+x)"))
#S(PARI:pari-ratfun :NUMER #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-1))
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 2 1)))
(pari:pari-to-lisp (pari:|_'| #Z"-1/(x^2 + 2*x + 1)"))
#S(PARI:pari-ratfun :NUMER #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(2))
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 3 3 1)))
(pari:pari-to-lisp (pari:|_'| #Z"2/(x^3 + 3*x^2 + 3*x + 1)"))
#S(PARI:pari-ratfun :NUMER #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-6))
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 4 6 4 1)))
(pari:pari-to-lisp (pari:|_'| #Z"-6/(x^4 + 4*x^3 + 6*x^2 + 4*x + 1)"))
#S(PARI:pari-ratfun :NUMER #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(24))
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 5 10 10 5 1)))
(pari:pari-to-lisp (pari:intformal #Z"-6/(x^4 + 4*x^3 + 6*x^2 + 4*x + 1)"))
#S(PARI:pari-ratfun :NUMER 2
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 3 3 1)))
(pari:pari-to-lisp (pari:intformal #Z"2/(x^3 + 3*x^2 + 3*x + 1)"))
#S(PARI:pari-ratfun :NUMER -1
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 2 1)))
(pari:pari-to-lisp (pari:intformal #Z"-1/(x^2 + 2*x + 1)"))
#S(PARI:pari-ratfun :NUMER 1
   :DENOM #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(1 1)))

(pari:pari-to-lisp (pari:taylor #Z"1/(1+x)^2" -1 :d 7))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 0 :COEFFS #(1 -2 3 -4 5 -6 7))
(pari:pari-to-lisp (pari:taylor #Z"(1-x)^2/(1+x)^2" -1 :d 7))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 0 :COEFFS #(1 -4 8 -12 16 -20 24))
(pari:pari-to-lisp (pari:serreverse #S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 1 :COEFFS #(1 -4 8 -12 16 -20 24))))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 1 :COEFFS #(1 4 24 172 1360 11444 100520))
(pari:pari-to-lisp (pari:taylor #Z"x/(1+x)^2" -1 :d 7))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 1 :COEFFS #(1 -2 3 -4 5 -6))
(pari:pari-to-lisp (pari:serreverse (pari:taylor #Z"x/(1+x)^3" -1 :d 7)))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 1 :COEFFS #(1 3 12 55 273 1428))
(pari:pari-to-lisp (pari:_*_ #Z"1+x" (pari:taylor #Z"1/(1+x)" -1 :d 7)))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 0 :COEFFS #(1 0 0 0 0 0 0))
(pari:pari-to-lisp (pari:serreverse #Z"sin(x)"))
#S(PARI:pari-pws :S 1 :VARNO 0 :EXPO 1 :COEFFS
   #(1 0 1/6 0 3/40 0 5/112 0 35/1152 0 63/2816 0 231/13312 0 143/10240 0))

(pari:pari-to-lisp (pari:quadunit 17))
#S(PARI:pari-quadratic :POLY #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-4 -1 1))
   :REALPART 3 :IMAGPART 2)
(pari:pari-to-lisp (pari:quadunit 512))
#S(PARI:pari-quadratic :REALPART 577 :IMAGPART 51
   :POLY #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-128 0 1)))
(pari:pari-to-lisp (pari:quadunit 513))
#S(PARI:pari-quadratic :REALPART 13163331 :IMAGPART 1216040
   :POLY #S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-128 -1 1)))
(pari:pari-to-lisp (pari:quadregulator 5)) 0.4812118250596034475L0
(pari:pari-to-lisp (pari:quadregulator 512)) 7.0509886961563442024L0
(pari:pari-to-lisp (pari:quadpoly 513))
#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-128 -1 1))
(pari:pari-to-lisp (pari:quadpoly 512))
#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-128 0 1))
(pari:pari-to-lisp (pari:quadpoly 5))
#S(PARI:pari-poly :S 1 :VARNO 0 :COEFFS #(-1 -1 1))
(pari:pari-to-lisp (pari:quaddisc 5)) 5
(pari:pari-to-lisp (pari:quaddisc 512)) 8
(pari:pari-to-lisp (pari:quaddisc 513)) 57

;; Equality is not transitive!!!!
(pari:gequal #(:COL) #(:ROW)) NIL ; sure
(pari:gequal #(:COL) 0) T         ; wtf?
(pari:gequal #(:ROW) 0) T         ; wtf?
(pari:isexactzero #(:COL)) T      ; wtf?
(pari:isexactzero #(:ROW)) T      ; wtf?
;; Not to worry! === is transitive!
(pari:_===_ #(:COL) #(:ROW)) 0
(pari:_===_ #(:COL) 0) 0
(pari:_===_ #(:ROW) 0) 0

(pari:gequal (pari:subst_ #Z"x^2+1" -1 #Z"y") #Z"x^2 + 1") T
(pari:gequal (pari:subst_ #Z"x^2+1" 0 #Z"y") #Z"y^2 + 1") T
(pari:pari-to-lisp (pari:serprec #Z"x + O(y^2)" 0)) (+ PARI:INFINITY)
(pari:pari-to-lisp (pari:serprec #Z"x + O(y^2)" 1)) 2
(pari:gequal (pari:serprec #Z"x + O(y^2)" -1) (pari:-_ '(- PARI:INFINITY))) T

;; clear the stack
(progn (show pari:pari-avma) (setq pari:pari-avma (show ltop)) nil) NIL

(defparameter qfi (show (pari:pari-to-lisp (pari:Qfb 1 2 3)))) QFI
(pari:sizeword qfi) 13
(pari:sizebyte qfi) 104
(pari:pari-to-lisp (pari:qfbcompraw qfi qfi))
#S(PARI:pari-imag-qf :A 1 :B 2 :C 3)
(pari:pari-to-lisp (pari:qfbred qfi))
#S(PARI:pari-imag-qf :A 1 :B 0 :C 2)

(defparameter qfr (show (pari:pari-to-lisp (pari:Qfb 1 7 3)))) QFR
(pari:sizeword qfr) 16
(pari:sizebyte qfr) 128
;; https://pari.math.u-bordeaux.fr/pub/pari/manuals/2.9.0/libpari.pdf:
;;   Unfortunately, t_QFRs are very inefficient, and are only provided
;;   for backward compatibility.
(pari:pari-to-lisp (pari:qfbcompraw qfr qfr))
#S(PARI:pari-real-qf :A 1 :B 7 :C 3 :D 0)
;; PARI stack overflows (pari:pari-to-lisp (pari:qfbred qfr))
(pari:pari-to-lisp (pari:qfbred qfr :flag 3))
#S(PARI:pari-real-qf :A 3 :B 5 :C -1 :D 0)

(pari:polsturm #Z"x^2-1") 2
(pari:polsturm #Z"x^2-1" :a 0) 1
(pari:polsturm #Z"x^2-1" :b 0) 1
;; interval is [b;a] !
(pari:polsturm #Z"x^2-1" :a 0 :b 2) 0
(pari:polsturm #Z"x^2-1" :b 0 :a 2) 1

;; check that no functionality is duplicated
(let ((gp2l (make-hash-table :test 'equal)) bad)
  (do-external-symbols (es "PARI")
    (let ((gp-name (documentation es 'pari::gp)))
      (when gp-name
        (push es (gethash gp-name gp2l)))))
  (maphash (lambda (gp-name esl)
             (when (cdr esl)
               (push (cons gp-name esl) bad)))
           gp2l)
  (dolist (cell bad)
    (setf (cdr cell) (sort (cdr cell) #'string<=)))
  (sort bad #'string<= :key #'car))
;; variants mentioned in pari.desc without their own entry
(("_/_" PARI:ginv PARI:_/_)
 ("_==_" PARI:gequal PARI:gequal0 PARI:gequal1 PARI:gequalm1 PARI:_==_)
 ("charpoly" PARI:caradj PARI:charpoly)
 ("nfeltpow" PARI:nfeltpow PARI:nfinv)
 ("random" PARI:ellrandom PARI:ffrandom PARI:random_)
 ("sizebyte" PARI:sizebyte PARI:sizeword)
 ("variables" PARI:variable-numbers PARI:variables))

;; check which functions have proper doc
(let ((pari-type-names (make-hash-table))
      (pari-type-functions (make-hash-table))
      (undocumented ()) (no-gp ()) (exported 0))
  (flet ((note (sym ht) (when sym (setf (gethash sym ht) sym))))
    (do-external-symbols (es "PARI")
      (let ((cl (find-class es nil)))
        (when cl
          (note es pari-type-names)
          (note (clos::class-kconstructor cl) pari-type-functions)
          (note (clos::class-copier cl) pari-type-functions)
          (note (clos::class-predicate cl) pari-type-functions)
          (dolist (slot (class-direct-slots cl))
            (dolist (reader (slot-definition-readers slot))
              (note reader pari-type-functions)))))))
  ;; (values pari-type-names pari-type-functions)
  (format t "~:D types, ~:D related functions~%"
          (hash-table-count pari-type-names)
          (hash-table-count pari-type-functions))
  (do-external-symbols (es "PARI")
    (let ((gp-name (documentation es 'pari::gp)))
      (incf exported)
      (if gp-name
          (unless (pari::get_entry_doc gp-name) ; missing for obsolete functions
            (push es undocumented)
            (format t "no gp doc for ~S ~S~%" es gp-name))
          (when (and (fboundp es) (not (gethash es pari-type-functions)))
            (push es no-gp)
            (format t "no pari function for ~S~%" es)))))
  (format t "~:D exported symbols, ~:D undocumented, ~:D without pari function~%"
          exported (length undocumented) (length no-gp))
  (list (sort no-gp #'string-lessp)
        (sort undocumented #'string-lessp)))
((PARI:gerepile PARI:maxprime PARI:next-entree PARI:pari-fini PARI:pari-init
  PARI:pari-real-precision PARI:pari-to-lisp PARI:pari-type)
 ;; This list should ideally contain only operators like * and ==
 ;; and tests like bigint? and complex?
 ;; All other functions might be accessing obsolescent functionality.
 (PARI:bigint? PARI:galois_group PARI:idealaddmultoone
  PARI:iscomplex PARI:isexactzero PARI:isinexact PARI:isinexactreal
  PARI:isint PARI:isrationalzero PARI:isrationalzeroscalar PARI:issmall
  PARI:pol_0 PARI:pol_1 PARI:pol_x
  PARI:primitive-part PARI:primpart PARI:scalarcol PARI:scalarmat PARI:varno))

;; done, print type conversion statistics
(let ((alist (sort (ext:with-collect (co)
                     (with-hash-table-iterator
                         (iter (get 'pari:pari-typecode 'ffi:def-c-enum))
                       (loop (multiple-value-bind (re kk vv) (iter)
                               (unless re (return))
                               (co (cons kk vv))))))
                   #'< :key #'car)))
  (flet ((show-coverage (title table)
           (format t "~A:~%" title)
           (dolist (pair alist)
             (format t " ~3D ~8A   ~:D~%" (car pair) (cdr pair)
                     (gethash (car pair) table)))))
    (format t "~&Type Conversion Coverage~%")
    (show-coverage "LISP --> PARI" *lisp-to-pari*)
    (show-coverage "PARI --> LISP" *pari-to-lisp*)))
NIL

(progn (without-package-lock ("PARI") (untrace))
       (setq *trace-output* *error-output*) ; re-enable TRACE, TIME, TIMES
       (symbols-cleanup '(*pari-to-lisp* *lisp-to-pari* roundtrip1 roundtrip2
                          roundtrip-1 roundtrip-2 mycol myrow picf
                          get-x-ash get-x-ash-neg check-roundtrip id qfi qfr)))
()
