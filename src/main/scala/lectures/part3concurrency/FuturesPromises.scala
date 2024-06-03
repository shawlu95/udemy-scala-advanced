package lectures.part3concurrency

import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure

// handle thread allocation for futures, passed as implicit param by compiler
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesPromises extends App {
  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  // calling apply() of the companion object of Future trait
  val aFuture = Future {
    // calculate the meaning of life on ANOTHER thread
    calculateMeaningOfLife
  } // (global) which is passed by the compiler

  // return Option[Try[Int]]
  // future may not have finished, and it could fail
  println(aFuture.value)
  println("waiting on the future")

  aFuture.onComplete(t => t match { // can simply to a partial function
    case Success(value) => println(s"the meaning of life is $value")
    case Failure(exception) => println(s"I failed with $exception")
  })

  Thread.sleep(3000)
}
