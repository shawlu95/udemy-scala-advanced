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
}
