package me.valekseev.heap
import cats.Order
import cats.instances.option._

/**
  * @author sss3 (Vladimir Alekseev)
  */
sealed trait ExplicitMinHeap[T]

object ExplicitMinHeap {

  def apply[T: Order, R[_]](implicit heap: Heap[T, R]): Heap[T, ExplicitMinHeap] = new EHeap(heap)

  private final case class ExpMinHeap[T, R[_]](min: Option[T], heap: R[T]) extends ExplicitMinHeap[T]

  private class EHeap[T: Order, R[_]](val h: Heap[T, R]) extends Heap[T, ExplicitMinHeap] {
    override def empty: ExplicitMinHeap[T] = ExpMinHeap(Option.empty, h.empty)

    override def isEmpty(heap: ExplicitMinHeap[T]): Boolean = heap match {
      case ExpMinHeap(None, _) => true
      case _                   => false
    }

    override def insert(x: T, heap: ExplicitMinHeap[T]): ExplicitMinHeap[T] = heap match {
      case ExpMinHeap(None, ih: R[T]) => ExpMinHeap(Option(x), h.insert(x, ih))
      case ExpMinHeap(m, ih: R[T])    => ExpMinHeap(Order[Option[T]].min(m, Option(x)), h.insert(x, ih))
    }

    override def merge(left: ExplicitMinHeap[T], right: ExplicitMinHeap[T]): ExplicitMinHeap[T] =
      (left, right) match {
        case (ExpMinHeap(m1, ih1: R[T]), ExpMinHeap(m2, ih2: R[T])) =>
          ExpMinHeap(Order[Option[T]].min(m1, m2), h.merge(ih1, ih2))
      }

    override def findMin(heap: ExplicitMinHeap[T]): Option[T] = heap match {
      case ExpMinHeap(m, _) => m
    }

    override def deleteMin(heap: ExplicitMinHeap[T]): ExplicitMinHeap[T] = heap match {
      case ExpMinHeap(_, ih: R[T]) =>
        val value = h.deleteMin(ih)
        ExpMinHeap(h.findMin(value), value)
    }

    override def ord: Order[T] = h.ord

  }
}
