package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.Try

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def count(h: H): Int =
    if (isEmpty(h)) 0
    else 1 + count(deleteMin(h))

  def getSeq(h: H): List[Int] =
    if (!isEmpty(h)) findMin(h) :: getSeq(deleteMin(h))
    else Nil

  def isOrderedSeq(l: List[Int]): Boolean = l match {
    case first :: second :: tail => first <= second && isOrderedSeq(second :: tail)
    case _ => true
  }

  def isOrdered(h: H): Boolean = isOrderedSeq(getSeq(h))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of any two element heap should return smallest of the two") = forAll { (i1: Int, i2: Int) =>
    val heap = insert(i1, insert(i2, empty))
    findMin(heap) == Math.min(i1, i2)
  }

  property("min of any one element heap should return the element") = forAll { (i: Int) =>
    findMin(insert(i, empty)) == i
  }

  property("deleting min from one-element heap returns empty heap") = forAll { (i: Int) =>
    isEmpty(deleteMin(insert(i, empty)))
  }

  property("deleting min removes 1 element from heap") = forAll { (h: H) =>
    (!isEmpty(h)) ==> (count(h) == count(deleteMin(h)) + 1)
  }

  property("deleting min removes min element from heap") = forAll { (h: H) =>
    (!isEmpty(h) && !isEmpty(deleteMin(h))) ==> (findMin(deleteMin(h)) >= findMin(h))
  }

  property("continually deleting smallest element returns sorted sequence") = forAll { (h: H) =>
    isOrdered(h)
  }

  property("minimum of two melded heaps returns a minimum of one or the other when at least one isn't empty") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) || !isEmpty(h2)) ==> {
      val min1 = if (isEmpty(h1)) None else Some(findMin(h1))
      val min2 = if (isEmpty(h2)) None else Some(findMin(h2))
      findMin(meld(h1, h2)) == (min1.toList ++ min2.toList).min
    }
  }

  property("insertion followed by deleting should yield empty heap") = forAll { (i: Int) =>
    deleteMin(insert(i, empty)) == empty
  }

  property("findMin of empty heap should throw exception") = Try(findMin(empty)).isFailure

  property("melding preserves ordering") = forAll { (h1: H, h2: H) =>
    isOrdered(meld(h1, h2))
  }

  property("insertion preserves ordering") = forAll { (i: Int, h: H) =>
    isOrdered(insert(i, h))
  }

  property("deleting item inserted after larger item inserted preserves structure") = forAll { (a: Int) =>
    (a < Int.MaxValue - 1) ==> {
      val h1 = insert(a + 1, insert(a, insert (a + 2, empty)))
      deleteMin(h1) == insert(a + 1, insert(a + 2, empty))
    }
  }

}
