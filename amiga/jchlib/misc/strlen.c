/* only loop over tst.b (a0)+ */
int strlen(const char* str)
{
  register const char* ptr = str;
  while (*ptr++) ;
  return ~(str - ptr);
}

/* almost identical to:
        move.l  (sp)+,a0
        move.l a0,d0
slloop:
        tst.l   (a0)+           ; Test for null-terminator and increment A0
        bne.b   slloop          ; Repeat if we didn't find the terminator
        not.l   d0              ; D0 = -D0 - 1
        add.l   a0,d0           ; D0 = A0 - D0 - 1
        rts
*/
