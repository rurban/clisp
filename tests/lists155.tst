
(MEMBER 'A
       '((A)
         (B)
         (A)
         (C)))
NIL

(MEMBER 'A
       '((A)
         (B)
         (A)
         (C))
       :KEY 'CAR)
((A)
 (B)
 (A)
 (C))

(MEMBER-IF 'NUMBERP
       '((A)
         (B)
         (3)
         (C))
       :KEY 'CAR)
((3)
 (C))

(MEMBER-IF-NOT 'NUMBERP
       '((8)
         (A)
         (B)
         (3)
         (C))
       :KEY 'CAR)
((A)
 (B)
 (3)
 (C))

(TAILP '(A B)
       '(U A B))
NIL

(TAILP (CDDR (SETQ XX
                   '(U I A B)))
       XX)
T

(TAILP (CDDR (SETQ XX
                   '(U I A B)))
       XX)
T

(ADJOIN 'A
       '(A B C))
(A B C)

(ADJOIN 'A
       '((A)
         B C)
       :TEST 'EQUAL)
(A (A)
   B C)

(ADJOIN 'A
       '((A)
         B C)
       :TEST 'EQUAL)
(A (A)
   B C)

(UNION '(A B C D)
       '(A D I V))
#+XCL (V I A B C D)
#+(or CLISP AKCL ECL) (B C A D I V)
#+(or ALLEGRO CMU SBCL OpenMCL) (C B A D I V)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(NUNION '(A B C D)
       '(U I B A))
#+XCL (A B C D U I)
#+(or CLISP AKCL ECL) (C D U I B A)
#+(or ALLEGRO CMU SBCL OpenMCL) (D C U I B A)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(NINTERSECTION '(A B C D)
       '(C D E F G))
#+(or XCL CLISP GCL ECL) (C D)
#+(or ALLEGRO CMU SBCL OpenMCL) (D C)
#-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(NINTERSECTION '(A B C D)
       '(C D E F G)
       :TEST-NOT 'EQL)
#+(or XCL CLISP GCL ECL) (A B C D)
#+(or ALLEGRO CMU SBCL OpenMCL) (D C B A)
#-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(SET-DIFFERENCE '(A B C D E)
       '(D B E))
#+(or XCL ALLEGRO GCL CMU SBCL OpenMCL) (C A)
#+(or CLISP (and AKCL (not GCL)) ECL) (A C)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(SET-DIFFERENCE '(AUTO ANTON BERTA BERLIN)
       '(A)
       :TEST
       #'(LAMBDA (X Y)
                (EQL (ELT (SYMBOL-NAME X)
                          1)
                     (ELT (SYMBOL-NAME Y)
                          1))))
#+(or XCL ALLEGRO) (BERLIN BERTA ANTON AUTO)
#-(or XCL ALLEGRO) ERROR

(SET-DIFFERENCE '(ANTON BERTA AUTO BERLIN)
       '(AMERILLA)
       :TEST
       #'(LAMBDA (X Y)
                (EQL (ELT (SYMBOL-NAME X)
                          0)
                     (ELT (SYMBOL-NAME Y)
                          0))))
#+(or XCL GCL ALLEGRO CMU SBCL OpenMCL) (BERLIN BERTA)
#+(or CLISP (and AKCL (not GCL)) ECL) (BERTA BERLIN)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(NSET-DIFFERENCE '(A B C D)
       '(I J C))
#+(or XCL CLISP GCL ECL) (A B D)
#+(or ALLEGRO CMU SBCL OpenMCL) (D B A)
#-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(SET-EXCLUSIVE-OR '(A B C D)
       '(C A I L))
#+(or XCL GCL) (D B L I)
#+(or CLISP (and AKCL (not GCL)) ECL) (B D I L)
#+(or ALLEGRO CMU SBCL OpenMCL) (L I D B)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(SET-EXCLUSIVE-OR '(ANTON ANNA EMIL)
       '(BERTA AUTO AUGUST)
       :TEST
       #'(LAMBDA (X Y)
                (EQL (ELT (SYMBOL-NAME X)
                          0)
                     (ELT (SYMBOL-NAME Y)
                          0))))
#+(or XCL CLISP GCL ECL) (EMIL BERTA)
#+(or ALLEGRO CMU SBCL OpenMCL) (BERTA EMIL)
#-(or XCL CLISP GCL ECL ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(NSET-EXCLUSIVE-OR '(A B C)
       '(I A D C))
#+OpenMCL (D I B)
#-OpenMCL (B I D)

(SUBSETP '(A B)
       '(B U I A C D))
T

(SUBSETP '(A B)
       '(B U I C D))
NIL

(SUBSETP '(A B)
       '(B A U I C D))
T

(SUBSETP '(A B)
       '(A U I C D))
NIL

