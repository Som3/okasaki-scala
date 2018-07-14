package me.valekseev.tree

import cats.{Order, Show}
import cats.syntax.show._
import me.valekseev.syntax.bst._
import me.valekseev.tree.BinarySearchTree._

/**
  * @author sss3 (Vladimir Alekseev)
  * Chapter 2.2
  */
trait FiniteMap[K, V, R[_, _]] {
  def empty: R[K, V]
  def bind(key: K, value: V, map: R[K, V]): R[K, V]
  def lookup(key: K, map: R[K, V]): Option[V]
}

object FiniteMap {

  sealed trait Map[K, V]
  private final case class Entry[K, V](key: K, value: Option[V])
  private final case class Nil[K, V]() extends Map[K, V]
  private final case class BSTMap[K, V](tree: Tree[Entry[K, V]]) extends Map[K, V]

  implicit def finiteMap[K: Order, V]: FiniteMap[K, V, Map] =
    new FiniteMapByBST[K, V]

  def apply[K, V, M[_, _]](implicit map: FiniteMap[K, V, M]): FiniteMap[K, V, M] = map

  private implicit def entryOrd[K: Order, V]: Order[Entry[K, V]] =
    (x: Entry[K, V], y: Entry[K, V]) => Order[K].compare(x.key, y.key)

  private class FiniteMapByBST[K, V](implicit tree: BinarySearchTree[Entry[K, V], Tree])
      extends FiniteMap[K, V, Map] {

    override def empty: Map[K, V] = Nil()

    override def bind(key: K, value: V, map: Map[K, V]): Map[K, V] = map match {
      case Nil() => BSTMap(tree.insert(Entry(key, Option(value)), tree.empty))
      case BSTMap(t) if tree.member(Entry(key, Option.empty[V]), t) =>
        BSTMap(tree.insert(Entry(key, Option(value)), tree.remove(Entry(key, Option.empty[V]), t)))
      case BSTMap(t) => BSTMap(tree.insert(Entry(key, Option(value)), t))
    }

    override def lookup(key: K, map: Map[K, V]): Option[V] = map match {
      case Nil()     => Option.empty[V]
      case BSTMap(t) => tree.find(Entry[K, V](key, Option.empty[V]), t).flatMap(_.value)
    }

  }

  implicit def show[K: Show: Order, V: Show]: Show[Map[K, V]] =
    new Show[Map[K, V]] {
      override def show(t: Map[K, V]): String = collect(t).mkString("[", ", ", "]")

      private def collect(t: Map[K, V]): Array[String] = t match {
        case Nil() => Array.empty[String]
        case BSTMap(tree) =>
          tree.foldLeft(Array.empty[String]) { (acc, e) =>
            acc :+ s"(${e.key.show} -> ${e.value.map(_.show).getOrElse("")})"
          }
      }
    }
}
