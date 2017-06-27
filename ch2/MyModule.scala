object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  private def formatAbs(x: Int) = {
    val msg = "The abs value of %d is %d"
    msg.format(x, abs(x))
  }

  //exercise 2.1
  def fib(n: Int) : Int = {
    def loop(n: Int, acc: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else loop(n-1, acc) + loop(n-2, acc)
    }

    loop(n, 0)
  }

  //exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  //exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => b: B => f(a,b)
  }

  //exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A,B) => C = {
     (a: A, b: B) =>f(a : A)(b : B)
  }

  //exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 5, factorial))
    println(formatResult("absolute value", -13, abs))
    print("findFirst('hello', ´l´) = ")
    println(findFirst(Array("h", "e", "l", "l", "o"), "l"))
    println(isSorted(Array(1,2,3,4), (x:Int, y:Int) => x < y ))
    println(formatResult("Fibonnaci", 10, fib))
  }
}