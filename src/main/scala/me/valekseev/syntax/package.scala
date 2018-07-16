package me.valekseev

import me.valekseev.stack.Stack
import me.valekseev.tree.BinarySearchTree

/**
  * @author sss3 (Vladimir Alekseev)
  */
package object syntax {
  object stack {
    implicit class StackOps[T, R[_]](v: R[T]) {

      def isEmpty(implicit stack: Stack[T, R]): Boolean = stack.isEmpty(v)

      def ::(x: T)(implicit stack: Stack[T, R]): R[T] = stack.cons(x, v)

      def head(implicit stack: Stack[T, R]): T = stack.head(v)

      def tail(implicit stack: Stack[T, R]): R[T] = stack.tail(v)

      def ++(r: R[T])(implicit stack: Stack[T, R]): R[T] = stack.++(v, r)

      def update(i: Int, t: T)(implicit stack: Stack[T, R]): R[T] = stack.update(v, i, t)
    }
  }

  object bst {
    implicit class BstOps[T, R[_]](v: R[T]) {

      def foldLeft[V](acc: V)(f: (V, T) => V)(implicit tree: BinarySearchTree[T, R]): V = tree.fold(v)(acc)(f)

    }
  }
}
