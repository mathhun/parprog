package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object Main extends App {
  val h = new QuickCheckHeap with quickcheck.BinomialHeap
  val hh = new QuickCheckHeap with quickcheck.Bogus4BinomialHeap

  val g0 = h.insert(-1, Nil)
  val gg0 = hh.insert(-1, Nil)
  println("")

  val h0 = (1 to 10).toList.foldLeft(g0) { (heap, n) => h.insert(n, heap) }
  println("min0: " + h.findMin(h0))
  val h1 = h.deleteMin(h0)
  println("min1: " + h.findMin(h1))
  println("")

  val hh0 = (1 to 10).toList.foldLeft(gg0) { (heap, n) => hh.insert(n, heap) }
  println("min0: " + hh.findMin(hh0))
  val hh1 = hh.deleteMin(hh0)
  println("min1: " + hh.findMin(hh1))
  println("")

  // List(
  //   Node(1,1,List(
  //     Node(10,0,List()))),
  //   Node(2,3,List(
  //     Node(4,2,List(
  //       Node(6,1,List(
  //         Node(7,0,List()))),
  //       Node(5,0,List()))),
  //     Node(8,1,List(
  //       Node(9,0,List()))),
  //     Node(3,0,List()))))

  // List(
  //   Node(8,1,List(
  //     Node(9,0,List()))),
  //   Node(-1,3,List(
  //     Node(4,2,List(
  //       Node(6,1,List(
  //         Node(7,0,List()))),
  //       Node(5,0,List()))),
  //     Node(2,1,List(
  //       Node(3,0,List()))),
  //     Node(1,0,List()))))
}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- frequency((1, const(empty)), (5, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("min3") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == math.max(a, b)
  }

  property("del1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  def toList(h: H): List[A] =
    if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))

  property("del2") = forAll { (h: H) =>
    val list = toList(h)
    list == list.sorted
  }

  property("del3") = forAll { (ns: List[Int]) =>
    val h = ns.foldLeft(empty) { (heap, n) => insert(n, heap) }
    toList(h) == ns.sorted
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h1: H, h2: H) =>
    val m1 = if (isEmpty(h1)) Int.MaxValue else findMin(h1)
    val m2 = if (isEmpty(h2)) Int.MaxValue else findMin(h2)

    findMin(meld(h1, h2)) == math.min(m1, m2)
  }
}
