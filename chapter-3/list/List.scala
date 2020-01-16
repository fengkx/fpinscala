sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*) : List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException("Nil.tail")
    case Cons(h, t) => t
  }

  def setHead[A](l :List[A] ,a: A): List[A] = l match {
    case Nil => Cons(a, Nil)
    case Cons(_, t) => Cons(a, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException("Nil.drop")
    case Cons(h, t) => 
      if(n == 0) l
      else drop(t, n -1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException("Nil.dropWhile")
    case Cons(h, t) => 
      if(!f(h)) l
      else dropWhile(t, f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IndexOutOfBoundsException("Nil.init")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as:List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((c, r) => r + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum2(as: List[Int]): Int = foldLeft(as, 0)((acc, cur) => acc + cur)
  def product2(as: List[Int]): Int = foldLeft(as, 1)((acc, cur) => acc * cur)
  def length2[A](as:List[A]) = foldLeft(as, 0)((acc, cur) => acc + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, cur) => Cons(cur, acc))

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(as), z)((b, a) => f(a,b))

  def append[A](as: List[A], r: List[A]) =
    foldRight(as, r)(Cons(_,_))

  def concat[A](ass: List[List[A]]) = 
    foldRight(ass, List[A]())(append(_,_))

  def addOne(as: List[Int]): List[Int] =
    foldRight(as, List[Int]())((cur: Int, acc: List[Int]) => Cons(cur+1, acc))
  
  def doubleToStr(as: List[Double]): List[String] =
    foldRight(as, List[String]())((cur: Double, acc: List[String]) => Cons(cur.toString, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((cur, acc) => Cons(f(cur), acc))
  
  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, List[A]())((cur, acc) =>
        if(f(cur)) Cons(cur,acc)
        else acc
        )

  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], r: List[A]): Boolean = {
      if(r == Nil) true
      else if(as== Nil) false
      else if(sup.head == r.head) go(sup.tail, sub.tail)
      else go(List.dropWhile(_ != r.head))
    }
    go(as, sub)
  }
}

val a = List(1,2,3)
println(List.tail(a))
println(List setHead(a, 123))
println(List drop(a, 2))
println(List dropWhile(a, (a:Int) => a<3))
println(List init a)
println(List length(a))
println(List sum2 a)
println(List reverse a)
println(List.concat(List(List(1,2),List(3,4),List(5,6))))
println(List addOne a)
println(List.map(a)(a => a + 1))
println(List.filter(a)(a => a < 2))
