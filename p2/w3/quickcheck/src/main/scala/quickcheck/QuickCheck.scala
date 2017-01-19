package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    def insertAll(values: List[Int], heap: H): H = {
      values match {
        case Nil => heap
        case head :: tail => insertAll(tail, insert(head, heap))
      }
    }
    for {
      list <- listOf(Gen.choose(0, 100))
    } yield {
      insertAll(list, empty)
    }
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of any two element heap should return smallest of the two") = forAll { (h: H) =>
    false
  }

  property("deleting min from one-element heap returns empty heap") = forAll { (h: H) =>
    false
  }

  property("continually deleting smallest element returns sorted sequence") = forAll { (h: H) =>
    false
  }

  property("minimum of two melded heaps returns a minimum of one or the other") = forAll { (h1: H, h2: H) =>
    false
  }

}
