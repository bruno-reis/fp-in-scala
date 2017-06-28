sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //exercise 3.11
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  //exercise 3.3
  def setHead[A](el: A, l: List[A]): List[A] = l match {
    case Nil => Cons(el, l)
    case Cons(x, xs) => Cons(el, xs)
  }

  //exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  //exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile (xs, f)
    case _ => l
  }

  //exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  //exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => 1 + y)
  }

  //exercise 3.12
//  def reverseFold[A](l: List[A]): List[A] = {
//    foldLeft(l, Nil)((x, xs) => Cons(xs, x))
//  }

  //exercise 3.16
//  def addOne[A](l: List[A]): List[A] = l match {
//    case Nil => Nil
//    case (x, xs) => Cons(x + 1, addOne(xs))
//  }


}

//exercise 3.1
  //1 + 2 = 3

