/* Compute the number of solutions to the n-queens problem on a nxn
   checkboard. */

/* dynamic data structures not needed for such a simple problem */
#define nmax 100

int queens (n)                /* function definition in traditional C style */
  int n;
{ /* Compute the solutions of the n-queens problem. Assume n>0, n<=nmax.
     We look for a function D:{1,...,n} -> {1,...,n} such that
     D, D+id, D-id are injective. We use backtracking on D(1),...,D(n).
     We use three arrays which contain information about which values
     are still available for D(i) resp. D(i)+i resp. D(i)-i. */
  int dtab[nmax]; /* values D(1),...D(n) */
  int freetab1[nmax+1]; /* contains 0 if available for D(i) in {1,...,n} */
  int freetab2[2*nmax+1]; /* contains 0 if available for D(i)+i in {2,...,2n} */
  int freetab3a[2*nmax-1]; /* contains 0 if available for D(i)-i in {-(n-1),...,n-1} */
#define freetab3 (&freetab3a[nmax-1])
  /* clear tables */
  { int i; for (i=1; i<=n; i++) { freetab1[i] = 0; } }
  { int i; for (i=2; i<=2*n; i++) { freetab2[i] = 0; } }
  { int i; for (i=-(n-1); i<n; i++) { freetab3[i] = 0; } }
 {int counter = 0;
  int i = 0; /* recursion depth */
  int* Dptr = &dtab[0]; /* points to next free D(i) */
  entry: /* enter recursion */
    i++;
    if (i > n)
      { counter++; }
      else
      { int try;
        for (try = 1; try <= n; try++)
          { if (freetab1[try]==0 && freetab2[try+i]==0 && freetab3[try-i]==0)
              { freetab1[try]=1; freetab2[try+i]=1; freetab3[try-i]=1;
                *Dptr++ = try;
                goto entry;
                comeback:
                try = *--Dptr;
                freetab1[try]=0; freetab2[try+i]=0; freetab3[try-i]=0;
      }   }   }
    i--;
    if (i>0) goto comeback;
  return counter;
}}

