package me.valekseev.heap

import cats.Order

import scala.annotation.tailrec

/**
  * @author sss3 (Vladimir Alekseev)
  */
trait Heap[T, R[_]] {

  def empty: R[T]
  def isEmpty(heap: R[T]): Boolean
  def insert(x: T, heap: R[T]): R[T]
  def merge(left: R[T], right: R[T]): R[T]
  def findMin(heap: R[T]): Option[T]
  def deleteMin(heap: R[T]): R[T]

  def ord: Order[T]
}

object Heap {

  def apply[T, R[_]](implicit heap: Heap[T, R]): Heap[T, R] = heap

  def fromList[R[_]]: FromListVia[R] = new FromListVia[R]

  class FromListVia[R[_]] {
    def apply[T: Order](list: List[T])(implicit heap: Heap[T, R]): R[T] = fromList(list.map(heap.insert(_, heap.empty)))

    @tailrec
    private def fromList[T](list: List[R[T]])(implicit heap: Heap[T, R]): R[T] =
      reduceList(list) match {
        case Nil           => heap.empty
        case x :: Nil      => x
        case x :: y :: Nil => heap.merge(x, y)
        case xs            => fromList(xs)
      }

    private def reduceList[T](list: List[R[T]])(
      implicit heap: Heap[T, R]): List[R[T]] = list match {
      case Nil         => list
      case x :: Nil    => x :: Nil
      case x :: y :: xs => heap.merge(x, y) :: reduceList(xs)
    }
  }

}
