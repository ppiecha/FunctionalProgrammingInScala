package chapter05

import chapter05.Stream.{cons, empty, unfold}

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  /*
  * EXERCISE 1: Write a function to convert a Stream to a List, which will
  * force its evaluation and let us look at it in the REPL. You can convert to the
  * regular List type in the standard library. You can place this and other functions
  * that accept a Stream inside the Stream trait.
  * */

  def toList: List[A] = uncons match {
    case None => Nil
    case Some((hd, tl)) => hd :: tl.toList
  }

  /*
  * EXERCISE 2: Write a function take for returning the first n elements of a Stream.
  * */

  def take(n: Int): Stream[A] = (uncons, n) match {
    case (None, _) => empty
    case (_, n) if n <= 0 => empty
    case (Some((hd, _)), 1) => cons(hd, empty)
    case (Some((hd, tl)), n) => cons(hd, tl.take(n - 1))
  }

  /*
  * EXERCISE 3: Write the function takeWhile for returning all starting
  * elements of a Stream that match the given predicate.
  * */

  def takeWhile(p: A => Boolean): Stream[A] = (uncons, p) match {
    case (None, _) => empty
    case (Some((hd, tl)), p) => if (p(hd)) cons(hd, tl.takeWhile(p)) else empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /*
  * EXERCISE 4: Implement forall, which checks that all elements in the
  * Stream match a given predicate. Your implementation should terminate the
  * traversal as soon as it encounters a non-matching value.
  * */

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /*
  * EXERCISE 5: Use foldRight to implement takeWhile. This will
  * construct a stream incrementally, and only if the values in the result are demanded
  * by some other expression.
  * */

  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else b)

  /*
  * EXERCISE 6: Implement map, filter, append and flatMap using foldRight.
  * */

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else b)

  def append[C >: A](c: C): Stream[C] =
    foldRight[Stream[C]](cons(c, empty))((a, b) => cons(a, b))

  def concat[C >: A](c: Stream[C]): Stream[C] =
    foldRight[Stream[C]](c)((a, b) => cons(a, b))

  def flatMap[C >: A](f: A => Stream[C]): Stream[C] =
    foldRight[Stream[C]](empty)((a, b) => f(a) concat b)

  /*
  * EXERCISE 12: Use unfold to implement map, take, takeWhile, zip (as in chapter 3) and zipAll.
  * */

  def mapUnfold[B >: A](f: A => B): Stream[B] = unfold(uncons) {
    case None => None
    case Some((hd, tl)) => Some(f(hd), tl.uncons)
  }

  def takeUnfold(n: Int): Stream[A] = unfold((uncons, n)) {
    case (None, _) => None
    case (_, 0) => None
    case (Some((hd, tl)), n) => Some(hd, (tl.uncons, n - 1))
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(uncons) {
    case None => None
    case Some((hd, tl)) => if (p(hd)) Some(hd, tl.uncons) else None
  }

  def zip[B](b: Stream[B]): Stream[(A, B)] = unfold(this.uncons -> b.uncons) {
    case (a, b) if a.isEmpty || b.isEmpty => None
    case (_, None) => None
    case (None, _) => None
    case (Some(a -> al), Some(b -> bl)) => Some(a -> b, al.uncons -> bl.uncons)
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] = {
    def noneIfEmpty[C](s: Option[(C, Stream[C])]): Option[Option[(C, Stream[C])]] =
      if (s.isEmpty) None
      else Some(s)
    unfold(Option(this.uncons) -> Option(b.uncons)) {
      case None -> None => None
      case (Some(Some(a -> al)), None) => Some(Some(a) -> None, noneIfEmpty(al.uncons) -> None)
      case (None, Some(Some(b -> bl))) => Some(None -> Some(b), None -> noneIfEmpty(bl.uncons))
      case (Some(Some(a -> al)), Some(Some(b -> bl))) =>
        Some(Some(a) -> Some(b), noneIfEmpty(al.uncons) -> noneIfEmpty(bl.uncons))
    }
  }

  /*
  * EXERCISE 14: implement using . For a given , tails unfold Stream
  * tails returns the of suffixes of the input sequence, starting with the Stream
  * original . So, given Stream Stream(1,2,3), it would return
  * Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty).
  * */

  def tails: Stream[Stream[A]] =
    unfold(this)(s => s.uncons match {
      case None => None
      case Some((_, tl)) => Some((s, tl))
    }) append empty

  /*
  * EXERCISE 15 (hard, optional): Generalize tails to the function scanRight,
  * which is like a foldRight that returns a stream of the intermediate results.
  * */

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight[(B, Stream[B])]((z, empty))((a, b) => {
      lazy val bb = b
      val c = f(a, bb._1)
      (c, cons(c, bb._2))
    })._2 append z
  }


}

object Stream extends App {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  assert(cons(1, cons(2, cons(3, empty))).toList == List(1, 2, 3))
  assert(cons(1, cons(2, cons(3, empty))).take(2).toList == List(1, 2))
  assert(cons(1, cons(2, cons(3, empty))).takeWhile(_ <= 2).toList == List(1, 2))
  assert(cons(1, cons(2, cons(3, empty))).forAll(_ >= 1))
  assert(!cons(1, cons(2, cons(3, empty))).forAll(_ <= 2))
  assert(cons(1, cons(2, cons(3, empty))).takeWhileViaFold(_ <= 2).toList == List(1, 2))
  assert(cons(1, cons(2, cons(3, empty))).map(_ % 2).toList == List(1, 0, 1))
  assert(cons(1, cons(2, cons(3, empty))).filter(_ % 2 == 1).toList == List(1, 3))
  assert(cons(1, cons(2, cons(3, empty))).append(4).toList == List(1, 2, 3, 4))
  assert(cons(1, cons(2, cons(3, empty))).concat(cons(4, empty)).toList == List(1, 2, 3, 4))
  assert(
    cons(1, cons(2, cons(3, empty)))
    .flatMap(x => cons(x, cons(x * x, empty)))
    .toList == List(1, 1, 2, 4, 3, 9)
  )

  lazy val ones: Stream[Int] = cons(1, ones)

  /*
  * EXERCISE 7: Generalize ones slightly to the function constant which
  * returns an infinite Stream of a given value.
  * */

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  assert(constant("a").take(3).toList == List("a", "a", "a"))

  /*
  * EXERCISE 8: Write a function that generates an infinite stream of integers,
  * starting from n, then n + 1, n + 2, etc
  * */

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  assert(from(1).take(3).toList == List(1, 2, 3))

  /*
  * EXERCISE 9: Write a function fibs that generates the infinite stream of
  * Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  * */

  def nextFib(prev: Int, curr: Int): Stream[Int] = cons(prev + curr, nextFib(curr, prev + curr))

  assert(nextFib(0, 1).take(4).toList == List(1, 2, 3, 5))

  lazy val fibs: Stream[Int] = cons(0, cons(1, nextFib(0, 1)))

  assert(fibs.take(12).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89))

  /*
  * EXERCISE 10: We can write a more general stream building function. It takes
  * an initial state, and a function for producing both the next state and the next value
  * in the generated stream. It is usually called:
  * */

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  /*
  * EXERCISE 11: Write fibs, from, constant and ones in terms of unfold.
  * */

  def fibsUnfold: Stream[Int] = unfold((0, 1)){case (a, b) => Some((a, (b, a + b)))}

  assert(fibsUnfold.take(12).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  assert(fromUnfold(1).take(3).toList == List(1, 2, 3))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  assert(constantUnfold("a").take(3).toList == List("a", "a", "a"))

  def onesUnfold: Stream[Int] = unfold(1)(x => Some(x, x))

  assert(onesUnfold.take(3).toList == List(1, 1, 1))

  /*
  * Test exercise 12
  * */

  assert(cons(1, cons(2, cons(3, empty))).mapUnfold(_ % 2).toList == List(1, 0, 1))
  assert(cons(1, cons(2, cons(3, empty))).takeUnfold(2).toList == List(1, 2))
  assert(cons(1, cons(2, cons(3, empty))).takeWhileUnfold(_ <= 2).toList == List(1, 2))
  val s1 = Stream(1, 2, 3, 4, 5)
  val s2 = Stream('a', 'b', 'c')
  assert((s1 zip s2).take(5).toList == List((1, 'a'), (2, 'b'), (3, 'c')))
  assert((s1 zipAll s2).take(5)
    .toList == List((Some(1),Some('a')), (Some(2),Some('b')), (Some(3),Some('c')), (Some(4),None), (Some(5),None)))

  /*
  * EXERCISE 13 (hard): implement startsWith using functions you've
  * written. It should check if one Stream is a prefix of another. For instance,
  * Stream(1,2,3) starsWith Stream(1,2) would be true.
  *
  * 1. loop through matching items
  * 2. check if rest matches
  * */

  def find[A](s: Stream[A], elem: A): Stream[A] = {
    if (s.isEmpty) empty
    else s.uncons match {
      case None => empty
      case Some((hd, tl)) => if (hd == elem) cons(hd, tl) else find(tl, elem)
    }
  }

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s2.uncons match {
      case None => false
      case Some((hd, _)) => find(s, hd).uncons match {
        case None => false
        case Some((h, t)) => (cons(h, t) zip s2).forAll(pair => pair._1 == pair._2) || startsWith(t, s2)
      }
    }

  assert(startsWith(Stream(1, 2, 3, 4, 5), Stream(3, 4, 5)))
  assert(startsWith(from(1), Stream(3, 4, 5)))

  /*
  * Test exercise 14
  * */

  assert(Stream(1, 2, 3).tails.take(4).toList.map(_.take(4).toList) ==
    List(List(1, 2, 3), List(2, 3), List(3), List())
  )

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (startsWith(_, s2))

  /*
  * Test exercise 15
  * */

  assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))


}
