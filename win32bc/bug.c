/* Demonstrate a bug in the preprocessor of Borland C++ 5.0 */

#define a_b 17
#define foo(x) bar(x)
#define do(c) foo(c##_b)
do(a)
/* correct expansion:      bar(17)   */
/* Borland 5.0 expansion:  bar(a_b)  */
