package lazyEval

/**
 * Created by krishna on 26/6/16.
 */

import Stream._

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    //can make this tail recursive
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    def tailRec(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => tailRec(t(), h() :: acc)
      case Empty => acc
    }
    tailRec(this, List())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile (p))
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, empty) else empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def headOption2: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Empty[B])((a, b) => cons(f(a), Empty))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty[B])((a, b) => f(a) append b)

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def ff(a: Int, b: Int): Stream[Int] = cons(a, ff(b, a + b))

    ff(0, 1)
  }

}
