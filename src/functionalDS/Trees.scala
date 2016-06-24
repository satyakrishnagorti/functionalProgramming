package functionalDS

/**
 * Created by krishna on 25/6/16.
 */
object Trees {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 1
    case Branch(a,b) => 1 + size(a) + size(b)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(a,b) => maximum(a) max maximum(b)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(x) => 0
    case Branch(a,b) => 1 + depth(a) max depth(b)
  }

  def map[A, B](t: Tree[A])(f: (A) => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(x,y) => Branch(map(x)(f), map(y)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(a, b) => g(fold(a)(f)(g), fold(b)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(x=>1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int = fold(t)(x=>x)(_ max _)

  def depth2(t: Tree[Int]): Int = fold(t)(x=>0)(1 + _ max _)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x=>Leaf(f(x)): Tree[B])((a,b)=>Branch(a,b))

  def main(args: Array[String]) = {
    val x = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(map2(x)(x=>x+1))
  }
}
