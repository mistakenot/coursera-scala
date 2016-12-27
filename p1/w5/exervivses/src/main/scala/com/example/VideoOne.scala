package scala.com.example

object VideoOne {
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def remove[T](xs: List[T], n: Int): List[T] = xs match {
    case List() => throw new Error("list is empty")
    case x :: xt => 
      if (n == 0) xt
      else if (n > 0) x :: remove(xt, n - 1)
      else throw new Error("n is out of range")
  }

  def flattern(xs: List[List[Any]]): List[Any] = xs match {
    case List() => List()
    case x :: xt => x ++ flattern(xt)
  } 
}