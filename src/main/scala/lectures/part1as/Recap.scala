package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {
  val aCondition: Boolean = false
  val aConditionedVal = if (aCondition) 1 else 0

  // imperative language uses instruction
  // scala uses expression to structure program
  val aBlock = {
    if (aCondition) 1
    0
  }

  // unit: does nothing meaningful, equivalent to void
  // e.g. changing value of variable, print to console
  val unit = println("hi, bailey")

  // function
  def aFunction(x: Int): Int = x + 1

  // tail recursion is converted to iteration, no stack overflow risk
  // @tailrec forces compiler to check
  @tailrec
  def factorial(n: Int, accumulator: Int): Int =
    if (n <= 0) accumulator
    else factorial(n - 1, n * accumulator)

  // ---
  // object oriented programming

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog // subtype polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("yummy")
  }

  // method notation
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog

  // operators are rewritten as method
  1 + 2
  1.+(2)

  // anonymous class, supply impl of a trait, immediately instantiate
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("eat a lot")
  }

  // generics
  abstract class MyList[+A] // covariance

  // singleton & companion pattern
  object MyList

  // case class
  // serializable, member fields, companion object with apply method
  case class Person(name: String, age: Int)

  // exceptions
  val throwException = throw new RuntimeException // Nothing type
  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "caught exception"
  } finally {
    println("side effect")
  }

  // ---
  // functional programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  incrementer(1)
  val anonymousIncrementer = (x: Int) => x + 1
  List(1, 2, 3).map(anonymousIncrementer)

  // for-comprehension
  val pairs = for {
    num <- List(1, 2, 3)
    char <- List('a', 'b', 'c')
  } yield num + "-" + char

  // Scala collections: Seqs, Arrays, Lists, Vectors, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "Jess" -> 555
  )

  // "collection": Options, Try
  val anOption = Some(2)

  // ---
  // pattern matching: most powerful feature
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case _ => x + "th"
  }
  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"$n says hi"
  }
}
