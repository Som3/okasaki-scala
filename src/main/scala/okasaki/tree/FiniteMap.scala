package okasaki.tree

import cats.Order

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

  sealed trait TreeMap[K, V]
  final case class Entry[K, V](key: K, value: Option[V])
  private final case class Nil[K, V]()                            extends TreeMap[K, V]
  private final case class TMap[K, V, R[_]](tree: R[Entry[K, V]]) extends TreeMap[K, V]

  implicit def `FiniteMap`[K: Order, V, R[_]](implicit tree: Tree[Entry[K, V], R]): FiniteMap[K, V, TreeMap] =
    new FiniteTMap[K, V, R]()

  implicit def entryOrd[K: Order, V]: Order[Entry[K, V]] =
    (x: Entry[K, V], y: Entry[K, V]) => Order[K].compare(x.key, y.key)

  private class FiniteTMap[K, V, R[_]](implicit tree: Tree[Entry[K, V], R]) extends FiniteMap[K, V, TreeMap] {

    override def empty: TreeMap[K, V] = Nil()

    override def bind(key: K, value: V, map: TreeMap[K, V]): TreeMap[K, V] = map match {
      case Nil() => TMap(tree.insert(Entry(key, Option(value)), tree.empty))
      case tm: TMap[K, V, R] if tree.member(Entry(key, Option.empty[V]), tm.tree) =>
        TMap(tree.insert(Entry(key, Option(value)), tree.remove(Entry(key, Option.empty[V]), tm.tree)))
      case tm: TMap[K, V, R] => TMap(tree.insert(Entry(key, Option(value)), tm.tree))
    }

    override def lookup(key: K, map: TreeMap[K, V]): Option[V] = map match {
      case Nil()             => Option.empty[V]
      case tm: TMap[K, V, R] => tree.find(Entry(key, Option.empty[V]), tm.tree).flatMap(_.value)
    }

  }
}
