package me.valekseev.stack

import java.util.NoSuchElementException

import cats.Show
import me.valekseev.syntax.stack._

/**
  * @author sss3 (Vladimir Alekseev)
  */
trait Stack[T, R[_]] {

  def empty: R[T]
  def isEmpty(stack: R[T]): Boolean
  def cons(t: T, stack: R[T]): R[T]
  def head(stack: R[T]): T
  def tail(stack: R[T]): R[T]
  def ++(left: R[T], right: R[T]): R[T]
  def update(r: R[T], i: Int, t: T): R[T]

}

object Stack {

  sealed trait CustomStack[T]
  final case class Nil[T]() extends CustomStack[T]
  final case class Cons[T](e: T, stack: CustomStack[T]) extends CustomStack[T]

  implicit def customStack[T]: Stack[T, CustomStack] =
    new Stack[T, CustomStack] {

      override def empty: CustomStack[T] = Nil()

      override def isEmpty(stack: CustomStack[T]): Boolean = stack match {
        case Nil() => true
        case _     => false
      }

      override def cons(t: T, stack: CustomStack[T]): CustomStack[T] =
        Cons(t, stack)

      override def head(stack: CustomStack[T]): T = stack match {
        case s if isEmpty(s) =>
          throw new NoSuchElementException // its not cool :(
        case Cons(e, _) => e
      }

      override def tail(stack: CustomStack[T]): CustomStack[T] = stack match {
        case n @ Nil()  => n
        case Cons(_, s) => s
      }

      override def ++(left: CustomStack[T], right: CustomStack[T]): CustomStack[T] = left match {
        case stack if isEmpty(stack) => right
        case _                       => cons(head(left), ++(tail(left), right))
      }

      override def update(r: CustomStack[T], i: Int, t: T): CustomStack[T] =
        r match {
          case stack if isEmpty(stack) => stack
          case _ =>
            i match {
              case 0            => cons(t, tail(r))
              case ix if ix > 0 => cons(head(r), update(tail(r), i - 1, t))
            }
        }
    }

  implicit def customStackShow[T: Show]: Show[CustomStack[T]] =
    (t: CustomStack[T]) => s"[${stackToString(t)}]"

  def stackToString[T: Show](stack: CustomStack[T]): String = stack match {
    case Nil() => ""
    case Cons(v, s) =>
      s match {
        case Nil() => Show[T].show(v)
        case _     => Show[T].show(v) + "," + stackToString(s)
      }
  }

  def apply[T, R[_]](implicit s: Stack[T, R]): Stack[T, R] = s

  def suffixes[T, R[_]](r: R[T])(implicit iS: Stack[T, R], rS: Stack[R[T], R]): R[R[T]] = r match {
    case s if iS.isEmpty(s) => rS.cons(s, rS.empty)
    case _                  => (r :: rS.empty) ++ suffixes(r.tail)
  }

}
