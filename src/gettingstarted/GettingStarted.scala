import scala.annotation.tailrec

/**
 * Created by krishna on 22/6/16.
 */
object GettingStarted{

  def fib(n: Int): Int = {
    @tailrec
    def tailrec(n: Int, prev: Int, curr: Int): Int = {
      if(n==0) 0
      else tailrec(n-1, curr, prev+curr)
    }
    tailrec(n,0,1)
  }

  def findFirst(l: List[Int], key: Int): Int = {
    def ff(l: List[Int], key: Int, index: Int): Int = {
      l match {
        case List() => -1
        case h::tail => if(h==key) index else ff(tail,key,index+1)
      }
    }
    ff(l,key,0)
  }

  def isSorted(l: List[Int]):Boolean = {
    l match {
      case List() => true
      case f::Nil => true
      case f::rest => if(f>=rest.head) false else isSorted(rest)
    }
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit ={
    //testing
    println(isSorted(List(1,2,3,4,5,6,7)))
    println(isSorted(List(1,2,6,5,6,7)))
    println(isSorted(List(1)))
  }
}