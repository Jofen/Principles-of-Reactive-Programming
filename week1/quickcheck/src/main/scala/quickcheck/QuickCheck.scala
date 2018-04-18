package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("min2") = forAll { (a: A, b: A) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }
  
  property("empty") = forAll { a: A =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }
  
  property("minoftwo") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val h3 = meld(h1, h2)
    findMin(h3) == Math.min(m1, m2)
  }
  
  property("sorted") = forAll { (h: H) =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || ((m <= findMin(h2)) && isSorted(h2))
      }
    }
    
    isSorted(h)
  }
  
  property("meld") = forAll { (h1: H, h2: H) =>
    def isEqual(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else
        (findMin(h1) == findMin(h2)) && isEqual(deleteMin(h1), deleteMin(h2))
    }
    val m1 = meld(h1, h2)
    val m2 = meld(deleteMin(h1), insert(findMin(h1), h2))
    isEqual(m1, m2)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    h <- oneOf(value(empty), genHeap)
  } yield insert(k, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
