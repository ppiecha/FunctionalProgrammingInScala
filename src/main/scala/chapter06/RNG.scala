package chapter06

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG extends App {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  /*
  * EXERCISE 1: Write a function to generate a random positive integer. Note:
  * you can use x.abs to take the absolute value of an Int, x. Make sure to handle
  * the corner case Int.MinValue, which doesn't have a positive counterpart
  * */

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (nextVal, nextRNG) = rng.nextInt
    if (nextVal == Int.MinValue) positiveInt(nextRNG) else (nextVal.abs, nextRNG)
  }

  val rng1 = simple(1)
  println(positiveInt(rng1))

  /*
  * EXERCISE 2: Write a function to generate a Double between 0 and 1, not
  * including 1. Note: you can use Int.MaxValue to obtain the maximum positive
  * integer value and you can use x.toDouble to convert an Int, x, to a Double.
  * */

  def double(rng: RNG): (Double, RNG) = {
    val (nextVal, nextRNG) = positiveInt(rng)
    (nextVal / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  println(double(rng1))

  /*
  * EXERCISE 3: Write functions to generate an (Int, Double) pair, a
  * (Double, Int) pair, and a (Double, Double, Double) 3-tuple. You
  * should be able to reuse the functions you've already written.
  * */

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextIntVal, nextRNG1) = rng.nextInt
    val (nextDoubleVal, nextRNG2) = double(nextRNG1)
    ((nextIntVal, nextDoubleVal), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextIntVal, nextRNG1) = rng.nextInt
    val (nextDoubleVal, nextRNG2) = double(nextRNG1)
    ((nextDoubleVal, nextIntVal), nextRNG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nextDoubleVal1, nextRNG1) = double(rng)
    val (nextDoubleVal2, nextRNG2) = double(nextRNG1)
    val (nextDoubleVal3, nextRNG3) = double(nextRNG2)
    ((nextDoubleVal1, nextDoubleVal2, nextDoubleVal3), nextRNG3)
  }

  /*
  * EXERCISE 4: Write a function to generate a list of random integers.
  * */

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int)(rng: RNG): List[(Int, RNG)] = count match {
      case n if n <= 0 => List()
      case n => (rng.nextInt._1, rng.nextInt._2) :: go(n - 1)(rng.nextInt._2)
    }
    val rngs = go(count)(rng)
    (rngs.map(_._1).to(List), rngs.last._2)
  }

  println(ints(3)(rng1))

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  println(int)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /*
  * EXERCISE 5: Use map to generate an between 0 and n, inclusive.
  * */

  def positiveMax(n: Int): Rand[Int] =
    map(positiveInt)(x => x % (n + 1))

  def positiveMaxGen(n: Int, seed: Long): Int = positiveMax(n)(simple(seed))._1

  println(positiveMaxGen(10, 123))
  println(positiveMax(10)(simple(234))._1)

  /*
  * EXERCISE 6: Use map to reimplement RNG.double in a more elegant way.
  * */

  def doubleViaMap(rng: RNG): (Double, RNG) = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  /*
  * EXERCISE 7: MAP2
  * */

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rc => {
      val (a, nextA) = ra(rc)
      val (b, nextB) = rb(nextA)
      (f(a, b), nextB)
    }
  }

  /*
  * EXERCISE 8: Implement sequence, for combining a List of transitions into a single transition.
  * */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil => rng => (List(), rng)
      case x :: xs => map2(x, sequence(xs))(_ :: _)
    }

  // Use it to reimplement the ints function you wrote before.

  def intsViaSeq(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  println(intsViaSeq(3)(rng1))

  /*
  * EXERCISE 9: Implement flatMap, then use it to reimplement positiveInt.
  * */

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (nextA, rng1) = f(rng)
    g(nextA)(rng1)
  }

  def positiveIntViaFlatMap(rng: RNG): (Int, RNG) = {
    flatMap(int)(x => r => if (x == Int.MinValue) positiveIntViaFlatMap(r) else (x.abs, r))(rng)
  }

  println(positiveIntViaFlatMap(rng1))

  /*
  * EXERCISE 10: Reimplement map and map2 in terms of flatMap.
  * */

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => rng => (f(a), rng))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))

  /**
   * EXERCISE 11: Generalize the functions unit, map, map2, flatMap, and sequence.
   * Add them as methods on the State case class where possible.
   * Otherwise you should put them in a State companion object
   *
   * def unit[A](a: A): Rand[A]
   * def map[A, B](s: Rand[A])(f: A => B): Rand[B]
   * def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
   * def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]
   * def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
   *
   */

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s2) = run(s)
        (f(a), s2)
      })

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      State(
        s => {
          val (a, s2) = run(s)
          val (b, s3) = sb.run(s2)
          (f(a, b), s3)
        }
      )

    def flatMap[B](g: A => State[S, B]): State[S, B] =
      State(
        s => {
          val (a, s2) = run(s)
          g(a).run(s2)
        }
      )

  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
      case Nil => State(s => (Nil, s))
      case h :: t => h.map2(sequence(t))(_ :: _)
    }

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()

  }

}
