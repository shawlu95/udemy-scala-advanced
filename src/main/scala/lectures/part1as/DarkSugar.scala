package lectures.part1as

import scala.util.Try

object DarkSugar extends App {
  // syntax sugar #1: method with single param
  def singleArgMethod(arg: Int): String = s"returns $arg"

  val description = singleArgMethod {
    // write some code that returns an int
    100
  }

  val aTryInstance = Try {
    throw new RuntimeException
  }

  List(1, 2, 3).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  // can reduce to lambda
  val anotherInstance: Action = (x: Int) => x + 1

  // example: Runnables. which can be passed to threads
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("some job")
  })
  val simplerThread = new Thread(() => println("scala way"))

  // also works for abstract class that only has one un-implemented method
  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }
  val anAbstractInstance: AnAbstractType = (a: Int) => println("nice")

  // syntax sugar #3: the :: and #:: methods
  // infix method would translates to 2.::(List(3, 4)), absurd!
  // compiler rewrites to List(3, 4).::(2)
  // scala spec: last char decides associativity of method. : is "right associative"
  val prependedList = 2 :: List(3, 4)
  1 :: 2 :: 3 :: List(4, 5)
  List(4, 5).::(3).::(2).::(1)

  class MyStream[T] {
    // declare a right-associative method
    def -->:(value: T): MyStream[T] = this // put actually impl here
  }
  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }
  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "scala is crazy"

  // syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Composite[Int, String] = ???
  val composite2: Int Composite String = ???

  class --> [A, B]
  val towards: Int --> String = ???

  // syntax sugar #6: update() is very special, much like apply()
  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // rewritten as anArray.update(2, 7)
  // update method is good for mutable collections

  // syntax sugar #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member: Int = internalMember // getter

    // space not allowed, must be member_=
    def member_=(value: Int): Unit =
      internalMember = value
  }

  val aMutable = new Mutable
  aMutable.member = 100 // rewritten as aMutable.member_=(42)
}
