package chapter04

import java.util.regex._
import scala.{Either => _, Option => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = this flatMap (x => if (f(x)) Some(x) else None)
}
case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Option extends App {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /*
  * EXERCISE 2: Implement the variance function (if the mean is m, variance
  * is the mean of math.pow(x - m, 2), in terms of mean and flatMap.
  * */

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map((x: Double) => math.pow(x - m, 2))))

  /*
  * EXERCISE 3: bothMatch is an instance of a more general pattern. Write a
  * generic function map2, that combines two values using a binary function. Option
  * If either value is , then the return value is too. Here is its signature:
  * */

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
  //  for {
  //    a1 <- a
  //    b1 <- b
  //  } yield f(a1, b1)

  /*
  * EXERCISE 4: Re-implement bothMatch above in terms of this new function,
  * to the extent possible.
  * */

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)


  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1).map(_(s)), mkMatcher(pat2).map(_(s)))(_ && _)
  }

  /*
  * EXERCISE 5: Write a function sequence, that combines a list of Options
  * into one option containing a list of all the Some values in the original list. If the
  * original list contains None even once, the result of the function should be None,
  * otherwise the result should be Some with a list of all the values. Here is its signature:
  * */

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
  }
  //  if (a.exists(_.orElse(None) == None)) None
  //  else Some(a map {case Some(x) => x})

  def sequenceFold[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((opt, opt_list) => map2(opt, opt_list)(_ :: _))

  assert(sequenceFold(List(Some(1), Some(2))) == Some(List(1, 2)))
  assert(sequenceFold(List(Some(1), None)) == None)

  /*
  * EXERCISE 6: Sometimes we will want to map over a list using a function that might fail,
  * returning None if applying it to any element of the list returns None.
  * */

  def traverseSimple[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequenceFold(a map f)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  assert(
    traverse(List(1, 2, 3)) {
        case x: Int if x % 2 == 0 => None
        case x => Some(x)
      } == None
  )

  assert(
    traverse(List(1, 2, 3)) {
      case x: Int if x < 0 => None
      case x => Some(x * x)
    } == Some(List(1, 4, 9))
  )

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)

  assert(sequenceViaTraverse(List(Some(1), Some(2))) == Some(List(1, 2)))
  assert(sequenceViaTraverse(List(Some(1), None)) == None)

}
