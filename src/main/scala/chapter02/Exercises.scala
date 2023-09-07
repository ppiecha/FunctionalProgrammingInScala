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

  /*
  * EXERCISE 3 (hard): Implement and write down a concrete usage partial1
  * of it. There is only one possible implementation that compiles.
  * */

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  /*
  * EXERCISE 4: Currying, which converts a currying function of N arguments
  * into a function of one argument that returns another function as its result
  * */

  def curry[A, B, C](f: (A, B) => C): A => B => C = x => f(x, _)

  /*
  * EXERCISE 5: Implement uncurry, which reverses the transformation of curry.
  * Note that => since associates to the right, curry => A => (B => C) can be written as A => B => C
  * */

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (x, y) => f(x)(y)

  /*
  * Exercise 6
  * Implement the higher-order function that composes two functions
  * */

  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

}
