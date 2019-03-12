package okasaki

import okasaki.stack.Stack

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

}
