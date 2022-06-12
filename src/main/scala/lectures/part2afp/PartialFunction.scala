package lectures.part2afp

object PartialFunction extends App {
  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  // we want to restrict x to be 0 ~ 5
  val aFussyFunc = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 43
    else throw new OutOfRangeException
  class OutOfRangeException extends RuntimeException

  // this idiom is a "total function", can't be assigned to partial func
  val aNicerFunc = (x: Int) => x match {
    case 1 => 42
    case 2 => 43
  } // {1, 2} => Int, "a partial function from Int to Int"

  // idiomatic way (still pattern matching underneath)
  val aPartialFunc: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 43
  } // "partial function value"
  println(aPartialFunc(2))

  // test if a partial func can be run for args
  println(aPartialFunc.isDefinedAt(3)) // false

  // lift: convert partial func to total func that returns option
  val lifted = aPartialFunc.lift // Int => Option[Int]
  println(lifted(2)) // Some(43)
  println(lifted(3)) // None

  // chaining partial func
  val pfChain = aPartialFunc.orElse[Int, Int] {
    case 3 => 0
  }
  println(pfChain(2)) // 43
  println(pfChain(3)) // 0

  // partial func extends normal func (subtype of total)
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  // HOF accept partial function
  val aMappedList = List(1, 2, 3).map {
    case 1 => 100
    case 2 => 200
    case 3 => 300
  }
  println(aMappedList)

  // partial function can only have one param type

  /**
   * Exercise
   * 1. construct a partial function instance yourself
   * 2. implement a chatbot that responds to a set of questions
   */
  val anotherFussyFunc = new PartialFunction[Int, Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 100
      case 2 => 200
      case 3 => 300
    }

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x ==3
  }

  val bot: PartialFunction[String, String] = {
    case "hello" => "I am a bot"
    case "bye" => "ok"
  }
  scala.io.Source.stdin.getLines().map(bot).foreach(println)
}
