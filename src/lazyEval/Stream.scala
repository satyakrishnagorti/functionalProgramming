package lazyEval

/**
 * Created by krishna on 26/6/16.
 */

import Stream._

sealed trait Stream[+A]{

  def toList: List[A] = this match{ //can make this tail recursive
    case Empty => List()
    case Cons(h, t) => h()::t().toList
  }

  def toListTailRec: List[A] = {
    def tailRec(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => tailRec(t(), h()::acc)
      case Empty => acc
    }
    tailRec(this, List())
  }

  def take(n: Int): Stream[A] = this match{
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }
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

}
