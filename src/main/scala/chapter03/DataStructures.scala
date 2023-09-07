/*
* TODO
* - move tests
* - fix compiler warnings
* */

package chapter03

object DataStructures extends App {

  /*
  * EXERCISE 2
  * Implement the function tail for "removing" the first element of a List. Notice the function takes constant time.
  * */

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case _ :: xs => xs
  }

  assert(tail(List(1, 2)) == List(2))

  /*
  * EXERCISE 3
  * Generalize tail to the function drop, which removes the first n elements from a list
  * */

  import scala.annotation.tailrec

  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = (list, n) match {
    case (Nil, _) => Nil
    case (l, 0) => l
    case (x :: xs, n) => drop(xs, n - 1)
  }

  assert(drop(List(1, 2, 3, 4), 2) == List(3, 4))

  /*
  * EXERCISE 4
  * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
  * */

  @tailrec
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case x :: xs => if (f(x)) dropWhile(xs)(f) else xs
  }

  assert(dropWhile(List(1, 2, 3, 4))(_ < 2) == List(3, 4))

  /*
  * EXERCISE 5
  * Implement the function setHead for replacing the first element of a with a different value
  * */

  def setHead[A](list: List[A])(a: A): List[A] = list match {
    case Nil => Nil
    case x :: xs => a :: xs
  }

  assert(setHead(List(1, 2, 3, 4))(5) == List(5, 2, 3, 4))

  /*
  * EXERCISE 6
  * Implement a function, init, which returns a List consisting of all but the last element of a List.
  * So, given List(1,2,3,4) init , will return List(1,2,3).
  * Why can't this function be implemented in constant time like tail?
  * */

  def init[A](list: List[A]): List[A] = {
    @tailrec
    def go(list: List[A], acc: List[A]): List[A] = (list, acc) match {
      case (Nil, acc) => Nil
      case (_ :: xs, acc) if xs.isEmpty => acc
      case (x :: xs, acc) => go(xs, x :: acc)
    }

    go(list, List()).reverse
  }

  assert(init(List()).isEmpty)
  assert(init(List(1)).isEmpty)
  assert(init(List(1, 2, 3, 4)) == List(1, 2, 3))

  /*
  * EXERCISE 7
  * Can product implemented using foldRight immediately halt the recursion and return 0.0 if it encounters a 0.0?
  * Why or why not?
  * */

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case x :: xs => f(x, foldRight(xs, z)(f))
    }

  assert(foldRight(List(1.0, 2.0, 0.0, 4.0), 1.0)(_ * _) == 0)

  // No. It must traverse all the way to the end before

  /*
  * EXERCISE 8
  * See what happens when you pass Nil and Cons themselves to foldRight
  * */

  assert(foldRight(List(1, 2, 3), Nil: List[Int])(_ :: _) == List(1, 2, 3))

  /*
  * EXERCISE 9
  * Compute the length of a list using foldRight
  * */

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, y) => y + 1)

  assert(length(List(1, 2, 3)) == 3)

  /*
  * EXERCISE 10
  * foldRight is not tail-recursive and will StackOverflow for large lists.
  * Convince yourself that this is the case, then write another general list-recursion function,
  * foldLeft that is tail-recursive
  * */

  //length(List.fill(100000)(1))

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  assert(foldLeft(List(1, 2, 3, 4), 0)(_ + _) == 10)

  /*
  * EXERCISE 11
  * Write sum, product, and a function to compute the length of a list using foldLeft.
  * */

  def sum(list: List[Int]): Int = list.foldLeft(0)(_ + _)

  assert(sum(List(1, 2, 3, 4)) == 10)

  def product(list: List[Double]): Double = list.foldLeft(1.0)(_ * _)

  assert(product(List(4, 2, 3)) == 24.0)
  assert(product(List()) == 1.0)

  def lengthViaFoldLeft[A](list: List[A]): Int = list.foldLeft(0)((x, _) => x + 1)

  assert(lengthViaFoldLeft(List()) == 0)
  assert(lengthViaFoldLeft(List(1, 2, 3)) == 3)

  /*
  * EXERCISE 12
  * Write a function that returns the reverse of a list (so given List(1,2,3) it returns List(3,2,1)).
  * See if you can write it using a fold
  * */

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def go(list: List[A], acc: List[A]): List[A] = (list, acc) match {
      case (Nil, acc) => acc
      case (x :: xs, acc) => go(xs, x :: acc)
    }
    go(list, Nil)
  }

  assert(reverse(List(1, 2, 3)) == List(3, 2, 1))

  def reverseFold[A](list: List[A]): List[A] = list.foldLeft(List[A]())((acc, x) => x :: acc)

  assert(reverseFold(List(1, 2, 3)) == List(3, 2, 1))

  /*
  * EXERCISE 13
  * Can you write foldLeft in terms of foldRight?
    How about the other way around?
  * */

  // 1 - List(2, 3).foldRight(0)(_ - _)
  // 1 - (2 - List(3).foldRight(0)(_ - _))
  // 1 - (2 - (3 - List().foldRight(0)(_ - _)))

  def foldRightViaLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =
    list.reverse.foldLeft(z)((x, y) => f(y, x))

  assert(foldRightViaLeft(List("a", "b", "c"), "")(_ + _) == "abc")

  /*
  * EXERCISE 14
  * Implement append in terms of either foldLeft or foldRight
  * */

  def appendFoldRight[A](list: List[A], a: A): List[A] = list.foldRight(List(a))(_ :: _)

  assert(appendFoldRight(List(1, 2, 3), 4) == List(1, 2, 3, 4))

  def appendFoldLeft[A](list: List[A], a: A): List[A] =
    list.reverse.foldLeft(List(a))((acc, x) => x :: acc)

  assert(appendFoldLeft(List(1, 2, 3), 4) == List(1, 2, 3, 4))

  /*
  * EXERCISE 15
  * Write a function that concatenates a list of lists into a single list.
  * Its runtime should be linear in the total length of all lists.
  * Try to use functions we have already defined.
  * */

  def flatten[A](list: List[List[A]]): List[A] = list.foldRight(List[A]())(_ ++ _)

  assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))

  def flatten2[A](list: List[List[A]]): List[A] = list match {
    case Nil => Nil
    case x :: xs => x ++ flatten2(xs)
  }

  assert(flatten2(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))

  /*
  * EXERCISE 16
  * Write a function that transforms a list of integers by adding 1 to each element.
  * (Reminder: this should be a pure function that returns a new List!)
  * */

  def add1(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case x :: xs => (x + 1) :: add1(xs)
  }

  assert(add1(List(1, 2)) == List(2, 3))

  /*
  * EXERCISE 17
  * Write a function that turns each value in a List[Double] into a String.
  * */

  def doubleToString(list: List[Double]): List[String] = list match {
    case Nil => Nil
    case x :: xs => x.toString :: doubleToString(xs)
  }

  assert(doubleToString(List(1, 2)) == List("1.0", "2.0"))

  /*
  * EXERCISE 18
  * Write a function map, that generalizes modifying each element in a list while maintaining the structure of the list
  * */

  def map[A, B](list: List[A], f: A => B): List[B] = list match {
    case Nil => Nil
    case x :: xs => f(x) :: map(xs, f)
  }

  assert(map[Int, Int](List(1, 2, 3), _ * 2) == List(2, 4, 6))

  /*
  * EXERCISE 19
  * Write a function filter that removes elements from a list unless they satisfy a given predicate.
  * Use it to remote all odd numbers from a List[Int]
  * */

  def filter[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case x :: xs =>
      val rest = filter(xs, f)
      if (f(x)) x :: rest else rest
  }

  assert(filter[Int](List(1, 2, 3, 4, 5, 6, 7), _ % 2 == 0) == List(2, 4, 6))

  /*
  * EXERCISE 20
  * Write a function flatMap, that works like map except that the function given
  * will return a list instead of a single result
  * and that list should be inserted into the final resulting list
  * */

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list match {
    case Nil => Nil
    case x :: xs => f(x) ::: flatMap(xs)(f)
  }

  assert(flatMap(List(1, 2, 3))(x => List(x, x * x)) == List(1, 1, 2, 4, 3, 9))
  assert(flatMap(List(1, 2, 3))(x => List(x, x)) == List(1, 1, 2, 2, 3, 3))

  /*
  * EXERCISE 21
  * Can you use flatMap to implement filter?
  * */

  def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] =
    flatMap(list)(x => if (f(x)) List(x) else Nil)

  assert(filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))

  /*
  * EXERCISE 22
  * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
  * For example, List(1,2,3) and List(4,5,6) becomes List(5,7,9)
  * */

  def zip(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match {
    case (Nil, Nil) => Nil
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) => x + y :: zip(xs, ys)
  }

  assert(zip(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  assert(zip(List(1, 2, 3), List(1, 2)) == List(2, 4))

  /*
  * EXERCISE 23
  * Generalize the function you just wrote so that it's not specific to integers or addition
  * */

  def zip[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = (list1, list2) match {
    case (Nil, Nil) => Nil
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) => f(x, y) :: zip(xs, ys)(f)
  }

  assert(zip(List(1, 2, 3), List("a", "b", "c"))(_.toString + _) == List("1a", "2b", "3c"))
  assert(zip(List(1), List("a", "b", "c"))(_.toString + _) == List("1a"))

  /*
  * EXERCISE 24
  * Implement hasSubsequence for checking whether a List contains another List as a subsequence
  * */

  def elemInList[A](list: List[A], elem: A): Option[Int] = {
    @tailrec
    def go(list: List[A], index: Int): Option[Int] = (list, index) match {
      case (Nil, _) => None
      case (x :: xs, index) => if (x == elem) Some(index) else go(xs, index + 1)
    }
    go(list, 0)
  }

  assert(elemInList(List(1, 2, 3), 2) == Option(1))
  assert(elemInList(List(1, 2, 3), 1) == Option(0))
  assert(elemInList(List(1, 2, 3), 2) == Option(1))
  assert(elemInList(List(1, 2, 3), 0).isEmpty)

  def getElemIndexes[A](list: List[A], elem: A): List[Int] = {
    def go(list: List[A], elem: A): (List[Int], Int) =
      list.foldLeft((List[Int](), 0)) {
        case ((acc, index), x) =>
          if (x == elem) (index :: acc, index + 1)
          else (acc, index + 1)
      }

    val (indexes, _) = go(list, elem)
    indexes.reverse
  }

  assert(getElemIndexes(List(1, 2, 3, 4, 2), 3) == List(2))
  assert(getElemIndexes(List(1, 2, 3, 4, 2), 2) == List(1, 4))
  assert(getElemIndexes(List(1, 2, 3, 4, 2), 0) == List())

  def isElemOnIndex[A](list: List[A], elem: A, index: Int): Boolean = list(index) == elem

  assert(isElemOnIndex(List(1, 2, 3), elem = 2, index = 1))
  assert(!isElemOnIndex(List(1, 2, 3), elem = 2, index = 0))

  @tailrec
  def areTheSame[A](list: List[A], sub: List[A]): Boolean = (list, sub) match {
    case (Nil, Nil) => true
    case (_, Nil) => true
    case (Nil, _) => false
    case (x :: xs, y :: ys) => x == y match {
      case true => areTheSame(xs, ys)
      case _ => false
    }
  }

  assert(areTheSame(List(1, 2, 3), List(1, 2, 3)))
  assert(!areTheSame(List(1, 2), List(1, 2, 3)))
  assert(areTheSame(List(1, 2, 3), List(1, 2)))

  // 0. having a list and a sub-list
  // 1. get list of indexes of first item from sub in list
  // 2. for each index from 1 check if rest of items matches

  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
    val indexes = getElemIndexes(list, sub.head)
    indexes.exists(index => areTheSame(list.drop(index + 1), sub.drop(1)))
  }

  assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
  assert(!hasSubsequence(List(1, 2, 3, 4), List(3, 2)))
  assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
  assert(hasSubsequence(List(1, 2, 1, 2, 3), List(1, 2, 3)))

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // EXERCISE 25: Write a function size that counts the number of nodes in a tree

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  assert(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)

  // EXERCISE 26: Write a function maximum that returns the maximum element in a Tree[Int]

  def maximum(tree: Tree[Int]): Int = {
    def go(tree: Tree[Int], acc: Option[Int]): Int = (tree, acc) match {
      case (Leaf(x), Some(acc)) => x max acc
      case (Leaf(x), None) => x
      case (Branch(left, right), acc) => go(left, acc) max go(right, acc)
    }
    go(tree, None)
  }

  assert(maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3)

  // EXERCISE 27: Write a function depth that returns the maximum path length from the root of a tree to any leaf

  def depth[A](tree: Tree[A]): Int = {
    def go(tree: Tree[A], acc: Int): Int = (tree, acc) match {
      case (Leaf(_), acc) => acc
      case (Branch(left, right), acc) => go(left, acc + 1) max go(right, acc + 1)
    }
    go(tree, 0)
  }

  val tree4Right = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  //       /  \
  //     1  /  \
  //      2  /  \
  //        3   4

  assert(depth(tree4Right) == 3)

  // EXERCISE 28: Write a function map, analogous to the method of the same name on List
  // that modifies each element in a tree with a given function

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left, f), map(right, f))
  }

  map[Int, Int](tree4Right, _ * 2)
  assert(map[Int, Int](tree4Right, _ * 2) == Branch(Leaf(2), Branch(Leaf(4), Branch(Leaf(6), Leaf(8)))))

  /*
  * EXERCISE 29: Generalize size, maximum, depth and map writing a new function fold
  * that abstracts over their similarities. Reimplement them in terms of
  * this more general function
  * */

  // t == fold(t)(Leaf(_), Branch(_, _))

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  val tree4Left =
    Branch(
      Branch(
        Branch(
          Leaf(1),
          Leaf(2)
        ),
        Leaf(3)
      ),
      Leaf(4)
    )

  //         /  \
  //        /  \ 4
  //       /  \ 3
  //      1    2

  def treeSize[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 1)(_ + _)

  assert(treeSize(tree4Left) == 4)

  def treeMax(tree: Tree[Int]): Int = fold[Int, Int](tree)(x => x)(_ max _)

  assert(treeMax(tree4Left) == 4)

  def treeDepth[A](tree: Tree[A]): Int = fold[A, Int](tree)(_ => 0)((l, r) => 1 + (l max r))

  assert(treeDepth(tree4Left) == 3)

  def treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(x => Leaf(f(x)))(Branch(_, _))

  assert(treeMap(tree4Left)(x => x * x) == Branch(Branch(Branch(Leaf(1),Leaf(4)),Leaf(9)),Leaf(16)))

}
