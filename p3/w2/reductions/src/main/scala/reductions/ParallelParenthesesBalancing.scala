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

    def traverse(idx: Int, until: Int, depthLeft: Int, depthRight: Int): (Int, Boolean) = {
      if (until - idx == 0) (depthLeft, depthRight)
        else if (chars(idx) == '(') traverse(idx + 1, until, depthLeft + 1, depthRight)
        else if (chars(idx) == ')') traverse(idx + 1, until, depthLeft, depthRight + 1)
        else traverse(idx + 1, until, depthLeft, depthRight)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else  {
        val halfway = (from + until) / 2
        val ((l1, r1), (l2, r2)) = parallel(reduce(from, halfway), reduce(halfway + 1, until))
        (l1 + l2, r1 + r2)
      }
    }

    val (l, r) = reduce(0, chars.length)
    l == r
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
