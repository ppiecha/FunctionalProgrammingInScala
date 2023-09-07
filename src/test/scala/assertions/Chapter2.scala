package assertions

import org.scalatest.flatspec.AnyFlatSpec
import chapter02.Exercises._

class Chapter2 extends AnyFlatSpec{

  "fib(n)" should "return n-th element of Fibonacci sequence" in {
    // 0 1 1 2 3 5 8
    val fibIndexes = Array[BigInt](1, 2, 3, 4, 5, 6, 7)
    val fib7Expected = Array[BigInt](0, 1, 1, 2, 3, 5, 8)
    val fib7Actual = fibIndexes.map(fib)
    assert(fib7Actual sameElements fib7Expected)
  }

  "isSorted" should "return True if Array is properly sorted" in {
    assert(isSorted[Int](Array(), (x, y) => x <= y))
    assert(isSorted[Int](Array(1), (x, y) => x <= y))
    assert(isSorted[Int](Array(1, 2), (x, y) => x <= y))
    assert(!isSorted[Int](Array(2, 1), (x, y) => x <= y))
    assert(isSorted[Int](Array(1, 2, 3), (x, y) => x <= y))
    assert(!isSorted[Int](Array(1, 3, 2), (x, y) => x <= y))
  }

  "partial1" should "return function with partially applied first parameter" in {
    val add1 = partial1[Int, Int, Int](1, (x, y) => x + y)
    assertResult(3, "add1(2) is expected to be 3")(add1(2))
  }

  "curry" should "return curried version of function" in {
    val curried = curry[Int, Int, Int]((x, y) => x + y)
    val add2 = curried(2)
    assert(add2(3) == 5)
  }

  "uncurry" should "return uncurried version of function" in {
    val testAdd = uncurry[Int, Int, Int](x => y => x + y)
    assert(testAdd(2, 1) == 3)
  }

  "compose" should "compose 2 functions" in {
    val toStr: Int => String = x => x.toString
    val toList: String => List[String] = x => List(x)
    val toStringList = compose(toList, toStr)
    assert(toStringList(2) == List("2"))
  }

}
