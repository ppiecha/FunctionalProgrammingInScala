package chapter04

/*
* EXERCISE 7: Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
* */

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

  def map2ViaFlatMap[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (a => b map (bb => f(a, bb)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

/*
* EXERCISE 8: Implement sequence and traverse for Either.
* */

object Either extends App {
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    a.foldRight[Either[E, List[A]]](Right(Nil))((x, y) => x.map2(y)(_ :: _))

  assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
  assert(sequence(List(Right(1), Left("other"))) == Left("other"))

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a.foldRight[Either[E, List[B]]](Right(Nil))((x, y) => f(x).map2(y)(_ :: _))

  def sequenceViaTraverse[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    traverse(a)(x => x)

  assert(
    traverse(List(1, 2, 3)) {
      case x: Int if x % 2 == 0 => Left("other")
      case x => Right(x)
    } == Left("other")
  )

  assert(
    traverse(List(1, 2, 3)) {
      case x: Int if x < 0 => Left("other")
      case x => Right(x * x)
    } == Right(List(1, 4, 9))
  )

  assert(sequenceViaTraverse(List(Right(1), Right(2))) == Right(List(1, 2)))
  assert(sequenceViaTraverse(List(Right(1), Left("other"))) == Left("other"))

}
