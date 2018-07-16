package me.valekseev.heap

import cats.Order
import cats.kernel.Comparison
import cats.syntax.order._

import scala.annotation.tailrec
import scala.collection.immutable.{Nil => SNil}

/**
  * @author sss3 (Vladimir Alekseev)
  */

trait LeftistHeap[T]

object LeftistHeap {

  private final case class Nil[T]() extends LeftistHeap[T]
  private final case class Node[T](rank: Int, x: T, left: LeftistHeap[T], right: LeftistHeap[T])
      extends LeftistHeap[T]

  implicit def heap[T: Order]: Heap[T, LeftistHeap] = new LHeap[T]

  def fromList[T: Order](list: List[T]): LeftistHeap[T] = fromList(list.map(Node(1, _, Nil(), Nil())))

  @tailrec
  private def fromList[T](list: List[LeftistHeap[T]])(implicit heap: Heap[T, LeftistHeap]): LeftistHeap[T] = reduceList(list) match {
    case SNil => heap.empty
    case x:: SNil => x
    case x::y::SNil => heap.merge(x, y)
    case xs => fromList(xs)
  }

  private def reduceList[T](list: List[LeftistHeap[T]])(implicit heap: Heap[T, LeftistHeap]): List[LeftistHeap[T]] = list match {
    case SNil => list
    case x::SNil => x :: SNil
    case x::y::xs => heap.merge(x, y) :: reduceList(xs)
  }

  private class LHeap[T: Order] extends Heap[T, LeftistHeap] {
    override def empty: LeftistHeap[T] = Nil()

    override def isEmpty(heap: LeftistHeap[T]): Boolean = heap match {
      case Nil() => true
      case _     => false
    }

    override def insert(x: T, heap: LeftistHeap[T]): LeftistHeap[T] = heap match {
      case Nil() => Node(1, x, Nil(), Nil())
      case n @ Node(ra, v, l, r) => x.comparison(v) match {
        case Comparison.LessThan => Node(1, x, n, Nil())
        case _ => rank(l).compareTo(rank(r)) match {
          case compare if compare > 0 =>
            val nR = insert(x, r)
            Node(rank(nR) + 1, v, l, nR)
          case _ => Node(ra, v, insert(x, l), r)
        }
      }
    }

    override def merge(left: LeftistHeap[T], right: LeftistHeap[T]): LeftistHeap[T] = (left, right) match {
      case (h, Nil()) => h
      case (Nil(), h) => h
      case (h1 @ Node(_, x, l1, r1), h2 @ Node(_, y, l2, r2)) =>
        x.comparison(y) match {
          case Comparison.GreaterThan => makeT(y, l2, merge(h1, r2))
          case _                      => makeT(x, l1, merge(r1, h2))
        }
    }

    override def findMin(heap: LeftistHeap[T]): Option[T] = heap match {
      case Node(_, x, _, _) => Option(x)
      case _                => Option.empty[T]
    }

    override def deleteMin(heap: LeftistHeap[T]): LeftistHeap[T] = heap match {
      case Node(_, _, l, r) => merge(l, r)
      case _                => heap
    }

    override def ord: Order[T] = Order[T]

    private def makeT(x: T, left: LeftistHeap[T], right: LeftistHeap[T]): LeftistHeap[T] =
      rank(left).compareTo(rank(right)) match {
        case compare if compare >= 0 => Node(rank(right) + 1, x, left, right)
        case _                       => Node(rank(left) + 1, x, right, left)
      }

    private def rank(h: LeftistHeap[T]): Int = h match {
      case Nil()            => 0
      case Node(r, _, _, _) => r
    }
  }

}