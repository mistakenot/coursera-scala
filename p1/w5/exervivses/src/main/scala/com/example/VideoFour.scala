package scala.com.example

object VideoFour {
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => List(x :: (xs1 takeWhile (_ == x))) ++ pack(xs1 dropWhile (_ == x))
  }

  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs) map {
      case x :: xt => (x, xt.length + 1)
    }
  }

  def repeat[T](x: T, n: Int): List[T] =
    if (n > 0) x :: repeat(x, n - 1)
    else if (n == 0) Nil
    else throw new Error("n < 0")

  def decode[T](xs: List[(T, Int)]): List[T] = xs match {
    case Nil => Nil
    case x :: xt => x match {
      case (t, num) => repeat(t, num) ++ decode(xt)
    }
  }
}