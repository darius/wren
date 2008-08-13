fun putuint base u =
  if ult (base-1) u then putuint base (udiv u base) else 0;
  putc *('0123456789abcdefghijklmnopqrstuvwxyz' + (umod u base))

fun putint base n =
  if n < 0 
  then (putc '-'; putuint base (-n))
  else putuint base n
