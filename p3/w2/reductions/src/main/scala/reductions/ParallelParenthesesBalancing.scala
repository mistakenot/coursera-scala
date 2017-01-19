package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def inner (depth: Int, c: List[Char]): Boolean =
        if (c.isEmpty) depth == 0
        else if (c.head == '(') inner(depth + 1, c.tail)
        else if (c.head == ')')
          if (depth == 0) false else inner(depth - 1, c.tail)
        else inner(depth, c.tail)

    inner(0, chars.toList)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, depth: Int): (Int, Boolean) =
      if (depth < 0) (depth, false)
      else if (until - idx == 0) (depth, depth == 0)
      else if (chars(idx) == '(') traverse(idx + 1, until, depth + 1)
      else if (chars(idx) == ')') traverse(idx + 1, until, depth - 1)
      else traverse(idx + 1, until, depth)

    def traverse2(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else {
        chars(idx) match {
          case '(' => traverse2(idx + 1, until, arg1 + 1, arg2)
          case ')' =>
            if (arg1 > 0) traverse2(idx + 1, until, arg1 - 1, arg2)
            else traverse2(idx + 1, until, arg1, arg2 + 1)
          case _ => traverse2(idx + 1, until, arg1, arg2)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse2(from, until, 0, 0)
      else  {
        val halfway = from + ((until - from) / 2)
        val ((lLeft, lRight), (rLeft, rRight)) = parallel(
          reduce(from, halfway),
          reduce(halfway, until))

        if (lLeft > rRight) (lLeft - rRight + rLeft, lRight)
        else (rLeft, rRight - lLeft + lRight)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
