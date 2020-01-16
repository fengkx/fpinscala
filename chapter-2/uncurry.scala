def uncurry[A,B,C](f: A => B => C) :(A,B) => C = {
  def func(a: A, b:B): C = {
    f(a)(b)
  }
  func
}
