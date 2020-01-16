def curry[A,B,C] (f: (A,B) => C): A => B => C = {
  def func(a: A): B => C = {
    def func2 (b: B): C = {
      f(a,b)
    }
    func2
  }
  func
}
