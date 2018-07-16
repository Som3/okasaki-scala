package me.valekseev.heap

import cats.Order
import cats.kernel.Comparison
import cats.syntax.order._
import cats.instances.int._
import me.valekseev.tree.BinarySearchTree.Tree

import scala.annotation.tailrec

/**
  * @author sss3 (Vladimir Alekseev)
  */
trait BinomialHeap[T]

object BinomialHeap {

  implicit def biHeap[T: Order]: Heap[T, BinomialHeap] = new BHeap[T]

  private final case class BHeapRep[T](tree: List[Node[T]]) extends BinomialHeap[T]
  private final case class Node[T](rank: Int, x: T, child: List[Node[T]])

  private class BHeap[T: Order] extends Heap[T, BinomialHeap] {
    override def empty: BinomialHeap[T] = BHeapRep(List.empty)

    override def isEmpty(heap: BinomialHeap[T]): Boolean = heap match {
      case BHeapRep(t) if t.isEmpty => true
      case _                        => false
    }

    override def insert(x: T, heap: BinomialHeap[T]): BinomialHeap[T] = insTree(Node(0, x, List.empty), heap)

    override def merge(left: BinomialHeap[T], right: BinomialHeap[T]): BinomialHeap[T] = (left, right) match {
      case (l, BHeapRep(Nil)) => l
      case (BHeapRep(Nil), r) => r
      case (lh @ BHeapRep(l :: ls), rh @ BHeapRep(r :: rs)) =>
        l.rank.comparison(r.rank) match {
          case Comparison.LessThan    => BHeapRep(l :: unwrap(merge(BHeapRep(ls), rh)))
          case Comparison.GreaterThan => BHeapRep(r :: unwrap(merge(lh, BHeapRep(rs))))
          case _                      => insTree(link(l, r), merge(BHeapRep(ls), BHeapRep(rs)))
        }
    }

    override def findMin(heap: BinomialHeap[T]): Option[T] = heap match {
      case BHeapRep(Nil) => Option.empty
      case h             => Option(removeMinTree(h)._1.x)
    }

    override def deleteMin(heap: BinomialHeap[T]): BinomialHeap[T] = heap match {
      case BHeapRep(Nil) => heap
      case h =>
        val r = removeMinTree(h)
        merge(BHeapRep(r._1.child), r._2)
    }

    override def ord: Order[T] = Order[T]

    @tailrec
    private def insTree(n: Node[T], heap: BinomialHeap[T]): BinomialHeap[T] = heap match {
      case BHeapRep(Nil) => BHeapRep(List(n))
      case BHeapRep(ts @ bh :: bhs) =>
        n.rank.compareTo(bh.rank) match {
          case c if c < 0 => BHeapRep(n :: ts)
          case _          => insTree(link(n, bh), BHeapRep(bhs))
        }
    }

    private def link(left: Node[T], right: Node[T]): Node[T] = (left, right) match {
      case (t1 @ Node(r, x1, c1), t2 @ Node(_, x2, c2)) =>
        x1.comparison(x2) match {
          case Comparison.GreaterThan =>
            Node(r + 1, x2, t1 :: c2)
          case _ => Node(r + 1, x1, t2 :: c1)
        }
    }

    private def unwrap(heap: BinomialHeap[T]): List[Node[T]] = heap match {
      case BHeapRep(xs) => xs
      case _            => List.empty
    }

    private def removeMinTree(heap: BinomialHeap[T]): (Node[T], BinomialHeap[T]) = heap match {
      case BHeapRep(x :: Nil) => (x, BHeapRep(Nil))
      case BHeapRep(x :: xs) =>
        val (t, ts) = removeMinTree(BHeapRep(xs))
        if (ord.lteqv(x.x, t.x)) { (x, BHeapRep(xs)) } else { (t, BHeapRep(x :: unwrap(ts))) }
    }

  }
}
