package functionalDS

import scala.annotation.tailrec

/**
 * Created by krishna on 23/6/16.
 */
object Lists {

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n==0) l
    if(l == Nil) Nil
    else drop(l.tail, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case a::rest if f(a) => dropWhile(rest,f)
      case _ => l
    }
  }

  def foldRight[A,B](l: List[A], z: B)(f:(B, A) => B): B =
    l match {
      case Nil => z
      case x::xs => f(foldRight(xs,z)(f), x)
    }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  def length[A](as: List[A]): Int = foldRight(as,0)((x,_)=>x+1)

  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, elem) => elem::acc)

  def foldRightUsingFoldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = foldLeft(reverse(l), z)((b,a)=>f(b,a))

  def append[A](l: List[A], m: List[A]): List[A] = foldRight(l, m)((acc,elem)=>elem::acc)

  def concat[A](l: List[List[A]]): List[A] = foldRight(l,List[A]())(append)

  def add1(l: List[Int]): List[Int] = foldRight(l,List[Int]())((acc, elem) => (elem+1)::acc)

  def doubleToString(l: List[Double]): List[String] = foldRight(l, List[String]())((acc, elem) => elem.toString::acc)

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())((acc, elem) => f(elem)::acc)

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]())((acc, elem) => if(f(elem)) elem::acc else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def addList(l: List[Int], m: List[Int]): List[Int] = (l, m) match {
    case (Nil , _ ) => Nil
    case (_ , Nil) => Nil
    case (x::xs, y::ys) => (x+y)::addList(l,m)
  }

  def zipWith[A, B, C](l: List[A], m: List[B])(f:(A,B)=>C): List[C] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x::xs, y::ys) => f(x,y)::zipWith(l,m)(f)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub==Nil
    case _ if sup.startsWith(sub) => true
    case x::xs => hasSubsequence(xs,sub)
  }

  def main(args: Array[String]) = {
    //testing
    println(foldRight(List(1,2,3,4),0)(_ + _))

    println(append(List(2,3,4,5), List(1,2,3)))
    println(append(List(),List()))

    println(concat(List(List(1,2,3),List(3,4,5))))

    println(add1(List(1,2,3)))
    println(doubleToString(List(1.0,2.2,3.3)))

    println(map(List(1,2,3))(x=>x*x))
  }

}