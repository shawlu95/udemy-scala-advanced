package lectures.part1as

object AdvancedPatternMatching extends App {
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ => // do nothing
  }

  /**
   * Recap: all pattern match cases
   * - constants
   * - wildcards
   * - case classes
   * - tuples
   * - special magic
   */

  // doesn't have to be a case class to pattern match
  // singleton object doesn't even have to match class name!
  class Person(val name: String, val age: Int)
  object PersonPattern {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "adult")
  }

  val shaw = new Person("Shaw", 27)
  val bailey = new Person("Bailey", 3)
  val greeting = shaw match {
    case PersonPattern(n, a) => s"Hi my name is $n and I am $a-years-old"
  }
  // val error = bailey match {...} // no match, return PatternMatchError
  println(greeting)

  val legalStatus = bailey.age match {
    case PersonPattern(status) => s"My legal status $status"
  }
  println(legalStatus)

  // ---
  // exercise
  object even {
    def unapply(arg: Int): Option[Boolean] =
      if (arg % 2 == 0) Some(true)
      else None
  }

  // doesn't have to return option
  object singleDigit {
    def unapply(arg: Int): Boolean = (arg > -10 && arg < 10)
  }

  val n: Int = 45
  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "even number"
    case _ => "don't care"
  }
  println(mathProperty)

  val property = n match {
    case even(_) => "even number"
    // doesn't need arg if return bool (not option)
    case singleDigit() => "single digit"
    case _ => "don't care"
  }
  println(property)

  // infix patterns, only work with two things
  case class Or[A, B](a: A, b: B) // either
  val either = Or(2, "Two")
  val humanDescription = either match {
    case Or(number, string) => s"$number is written as $string"
  }

  val anotherHunan = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)
  println(anotherHunan)

  // decomposing sequences
  var vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]
  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    // match against the Option[Seq[A]] returned by unapplySeq
    case MyList(1, 2, _*) => "starting with 1 and 2"
  }
  println(decomposed)

  // custom return types for unapply (doesn't have to be option)
  // required: return type must have isEmpty: Boolean, get: something
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get: String = person.name
    }
  }

  println(shaw match {
    case PersonWrapper(n) => s"This person's na me is $n"
    case _ => "alien"
  })
}
