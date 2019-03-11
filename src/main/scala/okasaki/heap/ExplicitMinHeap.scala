package okasaki.heap

import cats.Order
import cats.instances.option._
import cats.syntax.order._

/**
  * @author sss3 (Vladimir Alekseev)
  */
sealed trait ExplicitMinHeap[T]

object ExplicitMinHeap {

  implicit def `ExplicitMinHeap`[T: Order, R[_]](implicit heap: Heap[T, R]): Heap[T, ExplicitMinHeap] =
    new EHeap(heap)

  private final case class ExpMinHeap[T, R[_]](min: Option[T], heap: R[T]) extends ExplicitMinHeap[T]

  private class EHeap[T: Order, R[_]](val h: Heap[T, R]) extends Heap[T, ExplicitMinHeap] {
    override def empty: ExplicitMinHeap[T] = ExpMinHeap(Option.empty, h.empty)

    override def isEmpty(heap: ExplicitMinHeap[T]): Boolean = heap match {
      case ExpMinHeap(None, _) => true
      case _                   => false
    }

    override def insert(x: T, heap: ExplicitMinHeap[T]): ExplicitMinHeap[T] = heap match {
      case hh: ExpMinHeap[T, R] => internalInsert(x, hh)
    }

    override def merge(left: ExplicitMinHeap[T], right: ExplicitMinHeap[T]): ExplicitMinHeap[T] =
      (left, right) match {
        case (l: ExpMinHeap[T, R], r: ExpMinHeap[T, R]) => internalMerge(l, r)
      }

    override def findMin(heap: ExplicitMinHeap[T]): Option[T] = heap match {
      case ExpMinHeap(m, _) => m
    }

    override def deleteMin(heap: ExplicitMinHeap[T]): ExplicitMinHeap[T] = heap match {
      case hh: ExpMinHeap[T, R] =>
        val value = h.deleteMin(hh.heap)
        ExpMinHeap(h.findMin(value), value)
    }

    override def ord: Order[T] = h.ord

    private def internalInsert(x: T, heap: ExpMinHeap[T, R]) = heap match {
      case ExpMinHeap(None, ih) => ExpMinHeap(Option(x), h.insert(x, ih))
      case ExpMinHeap(m, ih)    => ExpMinHeap(Order[Option[T]].min(m, Option(x)), h.insert(x, ih))
    }

    private def internalMerge(left: ExpMinHeap[T, R], right: ExpMinHeap[T, R]) = (left, right) match {
      case (ExpMinHeap(m1, ih1), ExpMinHeap(m2, ih2)) =>
        ExpMinHeap(m1 min m2, h.merge(ih1, ih2))
    }

  }
}
