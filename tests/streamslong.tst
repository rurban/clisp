(READ-FROM-STRING "123") 123

(PRIN1-TO-STRING 123) "123"

(SETQ *A* (MAKE-ARRAY 10. :ELEMENT-TYPE 'STRING-CHAR :FILL-POINTER 0)) ""

(FORMAT *A* "XXX") NIL

*A* "XXX"

#+XCL (SYS::CHECK-STREAM-SYSTEM) #+XCL T

