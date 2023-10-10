package chapter07

import java.util.concurrent.{ExecutorService, Future}

object Par extends App {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A): Par[A] = s => s.submit(() => a)

  //def get[A](a: Par[A]): A = ???

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    s => s.submit(() => f(a(s).get, b(s).get))

  def fork[A](a: => Par[A]): Par[A] = s => s.submit(() => a(s).get())

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = {
    map2(l, unit(()))((a, _) => a.sorted)
  }

  def mapViaMap2[A, B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def sortPar2(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    s => s.submit(() => (fa(s).get, fb(s).get))

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = s => s.submit(() => f(fa(s).get))

  def map2ViaProduct[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    map(product(a, b))(pair => f(pair._1, pair._2))

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = l match {
    case Nil => unit(List[B]())
    case h :: t => s => s.submit(() => f(h) :: parMap(t)(f)(s).get)
  }

  def parMapViaFold[A,B](l: List[A])(f: A => B): Par[List[B]] =
    l.foldLeft(unit(List[B]()))((acc, elem) => s => s.submit(() => f(elem) :: acc(s).get))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List[A]()))((parA, acc) => map2(parA, acc)(_ :: _))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = l.map(asyncF((x: A) => if (f(x)) List(x) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    // a: es => Future[Boolean]
    es => {
      val par = map(a)((x: Boolean) => (if (x) ifTrue else ifFalse))(es).get
      par(es)
    }
  }

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(a).get)(es)

  def choiceViaN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    //choiceN(es => (if (a(es).get) unit(0) else unit(1))(es))(choices = List(ifTrue, ifFalse))
    choiceN(map(a)(x => if (x) 0 else 1))(choices = List(ifTrue, ifFalse))

  def choiceMap[A,B](a: Par[A])(choices: Map[A,Par[B]]): Par[B] =
    es => choices(run(es)(a).get)(es)

  def chooser[A,B](a: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(a).get)(es)

  def choiceViaChooser[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
    chooser(a)(x => if (x) ifTrue else ifFalse)

  def choiceNViaChooser[A](a: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(a)(int => choices(int))

  def join[A](a: Par[Par[A]]): Par[A] =
    es => a(es).get()(es)

  def flatMap[A, B](a: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(a).get)(es)

  def flatMapViaJoin[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(par => par)

}
