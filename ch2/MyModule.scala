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

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)

    loop(0)
  }

  private def formatAbs(x: Int) = {
    val msg = "The abs value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("factoriale", 5, factorial))
    println(formatResult("absolute value", -13, abs))
    findFirst(Array("h", "e", "l", "l", "o"), "l")
  }
}