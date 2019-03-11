package okasaki.tree

import cats.Order
import cats.kernel.Comparison
import cats.syntax.order._

import scala.annotation.tailrec

/**
  * @author sss3 (Vladimir Alekseev)
  * Chapter 2.2
  */
trait UnbalancedTree[T]

object UnbalancedTree {

  private final case class Node[T](left: UnbalancedTree[T], data: T, right: UnbalancedTree[T]) extends UnbalancedTree[T]
  private final case class Nil[T]() extends UnbalancedTree[T]

  implicit def tree[T: Order]: Tree[T, UnbalancedTree] = new UTree[T]

  private class UTree[T: Order] extends Tree[T, UnbalancedTree] {
    override def empty: UnbalancedTree[T] = Nil()

    override def insert(x: T, bst: UnbalancedTree[T]): UnbalancedTree[T] = bst match {
      case Nil() => Node(Nil(), x, Nil())
      case Node(l, v, r) =>
        x.comparison(v) match {
          case Comparison.EqualTo     => bst
          case Comparison.LessThan    => Node(insert(x, l), v, r)
          case Comparison.GreaterThan => Node(l, v, insert(x, r))
        }
    }

    override def find(x: T, bst: UnbalancedTree[T]): Option[T] = bst match {
      case Nil() => Option.empty[T]
      case Node(l, v, r) =>
        x.comparison(v) match {
          case Comparison.EqualTo     => Option(v)
          case Comparison.LessThan    => find(x, l)
          case Comparison.GreaterThan => find(x, r)
        }
    }

    override def member(x: T, bst: UnbalancedTree[T]): Boolean = find(x, bst).isDefined

    override def remove(x: T, bst: UnbalancedTree[T]): UnbalancedTree[T] = bst match {
      case Nil() => bst
      case Node(l, v, r) =>
        x.comparison(v) match {
          case Comparison.LessThan    => Node(remove(x, l), v, r)
          case Comparison.GreaterThan => Node(l, v, remove(x, r))
          case Comparison.EqualTo     => remove(l, r)
        }
    }

    private def remove(left: UnbalancedTree[T], right: UnbalancedTree[T]): UnbalancedTree[T] = (left, right) match {
      case (Nil(), r)             => r
      case (l, Nil())             => l
      case (l, Node(Nil(), v, r)) => Node(l, v, r)
      case (l, r) =>
        val minKey = min(r).data
        Node(l, minKey, remove(minKey, r))
    }

    @tailrec
    private def min(tree: UnbalancedTree[T]): Node[T] = tree match {
      case n @ Node(Nil(), _, _) => n
      case Node(l, _, _)         => min(l)
    }

  }
}
