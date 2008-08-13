fun putud n = (if 9 < n then putud (n/10) else 0); putc (*'0' + n%10)
fun count n = putud n; putc 10; if n then count (n-1) else 0
count 5000
