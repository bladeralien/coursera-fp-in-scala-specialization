package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(element, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get
  // the smallest of the two elements back.
  property("hint1") = forAll { (e1: A, e2: A) =>
    val min = if (e1 >= e2) e2 else e1
    findMin(insert(e2, insert(e1, empty))) == min
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("hint2") = forAll { (e: A) =>
    isEmpty(deleteMin(insert(e, empty)))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimal.
  // (Hint: recursion and helper functions are your friends.)
  property("hint3") = forAll { (h: H) =>
    def sorted(h: H): Boolean = {
        val smaller = deleteMin(h)
        if (isEmpty(smaller)) true
        else findMin(h) <= findMin(smaller) && sorted(smaller)
    }
    sorted(h)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("hint4") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = findMin(meld(h1, h2))
    m == m1 || m == m2
  }

  // https://github.com/TomLous/coursera-functional-program-design-in-scala/blob/master/src/main/scala/quickcheck/QuickCheck.scala
  property("inspired1") = forAll { (h1: H, h2: H) =>
    def identical(h1: H, h2: H): Boolean = {
      if (isEmpty(h1) && isEmpty(h2)) true
      else if ((isEmpty(h1) && !isEmpty(h2)) || (!isEmpty(h1) && isEmpty(h2))) false
      else findMin(h1) == findMin(h2) && identical(deleteMin(h1), deleteMin(h2))
    }
    identical(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("inspired2") = forAll { (h1: H, h2: H) =>
    import Math.min
    min(findMin(h1), findMin(h2)) == findMin(meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
