def fib(n: Int): Int = {
  @annotation.tailrec
  def go(r: Int, a: Int, b: Int): Int = {
    if(r == 0) a
    else if(r == 1) b
    else go(r-1, b, a+b)
  }
  go(n, 0, 1)
}

println(fib(5))
