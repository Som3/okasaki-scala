package me.valekseev.tree

import cats.Order
import cats.kernel.Comparison
import cats.syntax.order._

import scala.annotation.tailrec

/**
  * @author sss3 (Vladimir Alekseev)
  */
trait BinarySearchTree[T, R[_]] {

  def empty: R[T]
  def insert(x: T, bst: R[T]): R[T]
  def member(x: T, bst: R[T]): Boolean
  def find(x: T, bst: R[T]): Option[T]
  def remove(x: T, bst: R[T]): R[T]
  def traverse(bst: R[T])(f: T => Unit): Unit = foldLeft(bst)(())((_, v) => f(v))
  def foldLeft[V](bst: R[T])(acc: V)(f: (V, T) => V): V
  def split(x: T, bst: R[T]): (R[T], R[T])

}

object BinarySearchTree {
  sealed trait Tree[T]

  private final case class Node[T](left: Tree[T], data: T, right: Tree[T]) extends Tree[T]
  private final case class Nil[T]() extends Tree[T]

  implicit def tree[T: Order]: BinarySearchTree[T, Tree] = new UnbalancedTree[T]

  def apply[T, R[_]](implicit bst: BinarySearchTree[T, R]): BinarySearchTree[T, R] = bst

  private class UnbalancedTree[T](implicit ord: Order[T]) extends BinarySearchTree[T, Tree] {
    override def empty: Tree[T] = Nil()

    override def insert(x: T, bst: Tree[T]): Tree[T] = bst match {
      case Nil() => Node(Nil(), x, Nil())
      case Node(l, v, r) =>
        x.comparison(v) match {
          case Comparison.EqualTo     => bst
          case Comparison.LessThan    => Node(insert(x, l), v, r)
          case Comparison.GreaterThan => Node(l, v, insert(x, r))
        }
    }

    override def find(x: T, bst: Tree[T]): Option[T] = bst match {
      case Nil() => Option.empty[T]
      case Node(l, v, r) =>
        x.comparison(v) match {
          case Comparison.EqualTo     => Option(v)
          case Comparison.LessThan    => find(x, l)
          case Comparison.GreaterThan => find(x, r)
        }
    }

    override def member(x: T, bst: Tree[T]): Boolean = find(x, bst).isDefined

    override def remove(x: T, bst: Tree[T]): Tree[T] = bst match {
      case Nil() => bst
      case Node(l, v, r) =>
        x.comparison(v) match {
          case Comparison.LessThan    => Node(remove(x, l), v, r)
          case Comparison.GreaterThan => Node(l, v, remove(x, r))
          case Comparison.EqualTo     => remove(l, r)
        }
    }

    override def foldLeft[V](bst: Tree[T])(acc: V)(f: (V, T) => V): V = bst match {
      case Node(l, v, r) => foldLeft(r)(f(foldLeft(l)(acc)(f), v))(f)
      case Nil() => acc
    }

    override def split(x: T, bst: Tree[T]): (Tree[T], Tree[T]) = split(x, bst, Nil(), Nil())

    private def remove(left: Tree[T], right: Tree[T]): Tree[T] = (left, right) match {
      case (Nil(), Nil())         => Nil()
      case (Nil(), r)             => r
      case (l, Nil())             => l
      case (l, Node(Nil(), v, r)) => Node(l, v, r)
      case (l, r) =>
        val minKey = min(r).data
        Node(l, minKey, remove(minKey, r))
    }

    @tailrec
    private def min(tree: Tree[T]): Node[T] = tree match {
      case n @ Node(Nil(), _, _) => n
      case Node(l, _, _)         => min(l)
    }

    private def split(x: T, bst: Tree[T], left: Tree[T], right: Tree[T]): (Tree[T], Tree[T]) = bst match {
      case Nil() => (left, right)
      case Node(l, v, r) =>
        v.comparison(x) match {
          case Comparison.LessThan =>
            val tuple = split(x, l, insert(v, left), right)
            split(x, r, tuple._1, tuple._2)
          case _ =>
            val tuple = split(x, l, left, insert(v, right))
            split(x, r, tuple._1, tuple._2)
        }
    }

  }
}
