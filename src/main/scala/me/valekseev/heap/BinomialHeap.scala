package me.valekseev.heap

import cats.Order
import cats.kernel.Comparison
import cats.syntax.order._
import cats.instances.int._

import scala.annotation.tailrec

/**
  * @author sss3 (Vladimir Alekseev)
  */
trait BinomialHeap[T]

object BinomialHeap {

  implicit def biHeap[T: Order]: Heap[T, BinomialHeap] = new BHeap[T]

  private final case class BHeapRep[T](tree: List[Tree[T]]) extends BinomialHeap[T] {
    override def toString: String = s"BinomialHeap($tree)"
  }
  private final case class Node[T](x: T, child: List[Node[T]]) {
    override def toString: String = s"[$x, $child]"
  }
  private final case class Tree[T](rank: Int, root: Node[T])

  private class BHeap[T: Order] extends Heap[T, BinomialHeap] {
    override def empty: BinomialHeap[T] = BHeapRep(List.empty)

    override def isEmpty(heap: BinomialHeap[T]): Boolean = heap match {
      case BHeapRep(t) if t.isEmpty => true
      case _                        => false
    }

    override def insert(x: T, heap: BinomialHeap[T]): BinomialHeap[T] =
      insTree(Tree(0, Node(x, List.empty)), heap)

    override def merge(left: BinomialHeap[T], right: BinomialHeap[T]): BinomialHeap[T] = (left, right) match {
      case (l, BHeapRep(Nil)) => l
      case (BHeapRep(Nil), r) => r
      case (lh @ BHeapRep(l :: ls), rh @ BHeapRep(r :: rs)) =>
        l.rank.comparison(r.rank) match {
          case Comparison.LessThan    => BHeapRep(l :: unwrap(merge(BHeapRep(ls), rh)))
          case Comparison.GreaterThan => BHeapRep(r :: unwrap(merge(lh, BHeapRep(rs))))
          case _                      => insTree(Tree(l.rank + 1, link(l.root, r.root)), merge(BHeapRep(ls), BHeapRep(rs)))
        }
    }

    override def findMin(heap: BinomialHeap[T]): Option[T] = heap match {
      case BHeapRep(Nil) => Option.empty
      case BHeapRep(xs)  => Some(xs.map(_.root.x).min(ord.toOrdering))
    }

    override def deleteMin(heap: BinomialHeap[T]): BinomialHeap[T] = heap match {
      case BHeapRep(Nil) => heap
      case h =>
        val r = removeMinTree(h)
        val trees = r._1.root.child.zipWithIndex.map {
          case (n, i) => Tree(r._1.rank - i - 1, n)
        }.reverse
        merge(BHeapRep(trees), r._2)
    }

    override def ord: Order[T] = Order[T]

    @tailrec
    private def insTree(tree: Tree[T], heap: BinomialHeap[T]): BinomialHeap[T] = heap match {
      case BHeapRep(Nil) => BHeapRep(List(tree))
      case BHeapRep(ts @ bh :: bhs) =>
        tree.rank.compareTo(bh.rank) match {
          case c if c < 0 => BHeapRep(tree :: ts)
          case _          => insTree(Tree(tree.rank + 1, link(tree.root, bh.root)), BHeapRep(bhs))
        }
    }

    private def link(left: Node[T], right: Node[T]): Node[T] = (left, right) match {
      case (t1 @ Node(x1, c1), t2 @ Node(x2, c2)) =>
        x1.comparison(x2) match {
          case Comparison.GreaterThan =>
            Node(x2, t1 :: c2)
          case _ => Node(x1, t2 :: c1)
        }
    }

    private def unwrap(heap: BinomialHeap[T]): List[Tree[T]] = heap match {
      case BHeapRep(xs) => xs
      case _            => List.empty
    }

    private def removeMinTree(heap: BinomialHeap[T]): (Tree[T], BinomialHeap[T]) = heap match {
      case BHeapRep(x :: Nil) => (x, BHeapRep(Nil))
      case BHeapRep(x :: xs) =>
        val (t, ts) = removeMinTree(BHeapRep(xs))
        if (ord.lteqv(x.root.x, t.root.x)) { (x, BHeapRep(xs)) } else { (t, BHeapRep(x :: unwrap(ts))) }
    }
  }
}
