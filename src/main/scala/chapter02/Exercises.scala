/*
 * EXERCISE 1 (optional): Write a function to get the th Fibonacci number. The n
 * first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
 * the previous two. Your definition should use a local tail-recursive function
 */

package chapter02
import scala.annotation.tailrec

object Exercises extends App{

  /*
   * EXERCISE 1 (optional): Write a function to get the th Fibonacci number. The n
   * first two Fibonacci numbers are 0 and 1, and the next number is always the sum of
   * the previous two. Your definition should use a local tail-recursive function
   */

  def fib(n: BigInt): BigInt = {
    @tailrec
    def go(n: BigInt, current: BigInt, previous: BigInt): BigInt = {
      if (n == 1) previous
      else if (n == 2) current
      else go(n - 1, current + previous, current)
    }

    if (n == 1) 0 else go(n, 1, 0)
  }

  // 0 1 1 2 3 5 8
  val fibIndexes = Array[BigInt](1, 2, 3, 4, 5, 6, 7)
  val fib7Expected = Array[BigInt](0, 1, 1, 2, 3, 5, 8)
  val fib7Actual = fibIndexes.map(fib)
  assert(fib7Actual sameElements fib7Expected)

  /*
  * EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
  * sorted according to a given comparison function.
  * */

  @tailrec
  def isSorted[A](array: Array[A], lessThan: (A, A) => Boolean): Boolean = array match {
    case Array() => true
    case Array(_) => true
    case Array(x, rest@_*) => lessThan(x, rest.head) && isSorted(array.drop(1), lessThan)
  }

  assert(isSorted[Int](Array(), (x, y) => x <= y))
  assert(isSorted[Int](Array(1), (x, y) => x <= y))
  assert(isSorted[Int](Array(1, 2), (x, y) => x <= y))
  assert(!isSorted[Int](Array(2, 1), (x, y) => x <= y))
  assert(isSorted[Int](Array(1, 2, 3), (x, y) => x <= y))
  assert(!isSorted[Int](Array(1, 3, 2), (x, y) => x <= y))

  /*
  * EXERCISE 3 (hard): Implement and write down a concrete usage partial1
  * of it. There is only one possible implementation that compiles.
  * */

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  val add1 = partial1[Int, Int, Int](1, (x, y) => x + y)
  assert(add1(2) == 3)

  /*
  * EXERCISE 4: Currying, which converts a currying function of N arguments
  * into a function of one argument that returns another function as its result
  * */

  def curry[A, B, C](f: (A, B) => C): A => B => C = x => f(x, _)

  val curried = curry[Int, Int, Int]((x, y) => x + y)
  val add2 = curried(2)
  assert(add2(3) == 5)

  /*
  * EXERCISE 5: Implement uncurry, which reverses the transformation of curry.
  * Note that => since associates to the right, curry => A => (B => C) can be written as A => B => C
  * */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (x, y) => f(x)(y)

  val testAdd = uncurry[Int, Int, Int](x => y => x + y)
  assert(testAdd(2, 1) == 3)

  /*
  * Exercise 6
  * Implement the higher-order function that composes two functions
  * */

  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  val toStr: Int => String = x => x.toString
  val toList: String => List[String] = x => List(x)

  val toStringList = compose(toList, toStr)
  assert(toStringList(2) == List("2"))

}
