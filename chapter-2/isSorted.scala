import scala.util.Random

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def go(idx: Int): Boolean = {
    if(idx >= as.length - 1) true
    else if(ordered(as(idx), as(idx+1)))
      go(idx+1)
    else false
  }
  go(0)
}

val arr1 = Array(1,2,3,4,5,6,7)
val arr2 = Random.shuffle(arr1).toArray

def ordered(a: Int, b: Int) = a<b

println(isSorted(arr1, ordered))
println(isSorted(arr2, ordered))
