package lectures.part2afp

object Monad extends App {

  // custom try Monad
  trait Attempt[+A] { // make covariant
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    // call by name: lazy eval
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /**
   *  left identity: unit.flatMap(f) = f(x)
   *  Attempt(x).flatMap(f) = f(x) // success
   *  Success(x).flatMap(f) = f(x) // proved
   *
   *  right identity
   *  attempt.flatMap(unit) = attempt
   *  Success(x).flatMap(x => Attempt(x)) = Attempt(x) = Success(x)
   *  Fail(e).flatMap(...) = Fail(e)
   *
   *
   *  associativity
   *  attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
   *  Fail(e).flatMap(f).flatMap(g) = Fail(e)
   *  Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)
   *a
   *  Success(v).flatMap(f).flatMap(g) =
   *    f(v).flatMap(g) OR Fail(e)
   *
   *  Success(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g) OR Fail(e)
   */

  val attempt = Attempt {
    throw new RuntimeException("My monad")
  }

  println(attempt)

  /*
  Exercise:
  1) implement a Lazy[T] monad = computation which

  only excecutes when needed
    unit/apply
    flatMap

  2) Monads = unit + flatMap
     Monads = unit + map + flatten

  Given a correctly implemented Monad with flatMap
  Implement it using map and flatten instead.

  Monad[T] {
    def flatMap[B]{f: T => Monad[B]): Monad[B] = ... (implemented)

    def map[B}(f: T => B): Monad[B] = ???
    def flatten(m: Monad[Monad[T]]): Monad[T] = ???
  }
  */

  // pass value by name, which prevents evaluation when constructed
  class Lazy[+A](value: => A) {
    // call by need
    private lazy val internalValue = value
    def use: A = internalValue
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  // test Lazy monad
  val lazyInstance = Lazy {
    // should not see lyric printed when constructed
    println("Today I don't feel like doing anything")
    42
  }

  // should see both the lyric and 42
  //  print(lazyInstance.use)

  // ran into problem here, we see the lyric printed
  // even though it should not. Because flatMap is evaluated eagerly.

  // to fix, change function type to receive param by name as well:
  //   flatMap[B](f: A => Lazy[B]):
  // to:
  // flatMap[B](f: (=> A) => Lazy[B]):
  val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  // force evaluation, printed twice!
  // to fix, call by need
  // use "private lazy val internalValue = value"
  flatMappedInstance.use
  flatMappedInstance2.use

  /*
    left identity
    unit.flatMap(f) = f(v)
    Lazy(v).flatMap(f) = f(v)

    right-identity
    l.flatMap(unit) = l
    lazy(v).flatMap(x => Lazy(x)) = Lazy(v)

    associativity: l.flatMap(f).flatMap(g) = l.flatMap(x => f(x).flatMap(g))
    Lazy(v).flatMap(f).flatMap(g) = f(v).flatMap(g)
    lazy(v).flatMap(x => f(x).flatMap(g)) = f(v).flatMap(g)
  */

  // 2: map and flatten in terms of flatMap
  /*
    Monad[T] {
    def flatMap[B]{f: T => Monad[B]): Monad[B] = ... (implemented)

    def map[B}(f: T => B): Monad[B] = flatMap(x => unit(f(x))
    def flatten(m: Monad[Monad[T]]): Monad[T] = = m.flatMap((x: Monad(T) => x)
  }
  // we proved "unit + flatMap" is equivalent to "unit + map + flatten"
  list(1, 2, 3).map(X * 2) = List(1, 2, 3).flatMap(x => List(x * 2)(
  list(list(1, 2), list(3, 4)).flatten = list(list(1, 2), list(3, 4)).flatMap(x => x)
  */

}
