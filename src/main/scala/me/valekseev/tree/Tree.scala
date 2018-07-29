package me.valekseev.tree

/**
  * @author sss3 (Vladimir Alekseev)
  */
trait Tree[T, R[_]] {

  def empty: R[T]
  def insert(x: T, tree: R[T]): R[T]
  def member(x: T, tree: R[T]): Boolean
  def find(x: T, tree: R[T]): Option[T]
  def remove(x: T, tree: R[T]): R[T]

}

object Tree {
  def apply[T, R[_]](implicit tree: Tree[T, R]): Tree[T, R] = tree

  def fromList[R[_]] = new FromListVia[R]

  class FromListVia[R[_]] {
    def apply[T](list: List[T])(implicit tree: Tree[T, R]): R[T] = list.foldLeft(tree.empty)((t, v) => tree.insert(v, t))
  }
}