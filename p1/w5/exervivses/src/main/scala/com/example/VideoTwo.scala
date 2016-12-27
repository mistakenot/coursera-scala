package scala.com.example

object VideoTwo {
  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, y) => y
    case (x, Nil) => x
    case (x :: xt, y :: yt) =>
      if (x < y) x :: merge(xt, ys)
      else y :: merge(xs, yt) 
  }
}