package me.valekseev.heap

import cats.Order

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

}
