package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success, Try}

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

  // mini social network
  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) = println(s"${this.name} poking ${anotherProfile.name} ")
  }

  object SocialNetwork {
    // a map database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy"
    )
    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )
    val random = new Random()

    // simulate API fetch from database
    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriends(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      val id = friends(profile.id)
      Profile(id, names(id))
    }
  }

  // client-side code
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  mark.onComplete {
    case Success(markProfile) => {
      val bill = SocialNetwork.fetchBestFriends(markProfile)
      bill.onComplete {
        case Success(billProfile) => markProfile.poke(billProfile)
        case Failure(e) => e.printStackTrace()
      }
    }
    case Failure(e) => e.printStackTrace()
  }
  Thread.sleep(1000)

  // functional composition of futures
  // map, flatMap, filter
  val nameOnTheWall = mark.map(profile => profile.name) // Future[String]
  val marksBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriends(profile))
  val zucksBestFriendRestricted = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  // for comprehension
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriends(mark)
  } mark.poke(bill)

  Thread.sleep(1000)

  // fallbacks
  // return a newly constructed object
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Forever Alone") // return a placeholder profile
  }

  // we know "fb.id.0-dummy" must not fail, so we can fetch again
  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallBackResult = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // online banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM Banking"
    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some processes
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch user from the DB
      // create a transaction
      // WAIT for transaction to finish
      val txnStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      // 2.seconds due to implicit conversion: import scala.concurrent.duration._
      // block until future is completed, no need to Thread.sleep
      // if too slow, throw Timeout exception
      Await.result(txnStatusFuture, 2.seconds)
    }
  }

  println(BankingApp.purchase("Daniel", "iPhone 12", "Apple", 3000))

  // promises: as "controller" of future
  // no more concurrency issue
  // separate the concern of reading & writing to promise while elminating concurrency issue
  val promise = Promise[Int]()
  val future = promise.future

  // thread 1 - "consumer" knows how to handle future completion
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
    case Failure(e) => println(e.printStackTrace())
  }

  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crunching numbers")
    Thread.sleep(1000)
    // "fulfilling" the promise
    promise.success(42)
    println("[producer] done")

//    promise.failure(new RuntimeException("exception"))
//    println("[producer] failed")
  })

  producer.start()
  Thread.sleep(1000)

  /**
   * 1) fulfill a future immediately with a value
   * 2) inSequence(fa, fb)
   * 3) first(fa, fb) => new future that contains the first-completed future value
   * 4) last(fa, fb) => new future with the last-completed future value
   * 5) retryUntil(action: () => Future[T], condition: T => Boolean): Future[T]
   */
  // 1 - fulfill immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2 - in sequence
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = first.flatMap(_ => second)

  // 3 - first(fa, fb)
  def first[A](first: Future[A], second: Future[A]) = {
    val promise = Promise[A]

//    def tryComplete(promise: Promise[A], result: Try[A]): Unit = result match {
//      case Success(r) => try {
//        promise.success(r)
//      } catch {
//        case _ =>
//      }
//      case Failure(e) => try {
//        promise.failure(e)
//      } catch {
//        case _ =>
//      }
//    }
//    first.onComplete(result => tryComplete(promise, result))
//    second.onComplete(_ => tryComplete(promise, _))

    // even better, use the built in method
    first.onComplete(promise.tryComplete)
    second.onComplete(promise.tryComplete)
    promise.future
  }

  // 4 - last completed future
  def last[A](first: Future[A], second: Future[A]) = {
    // both futures try to complete this promise
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    val checkAndComplete = (result: Try[A]) =>
      // the last-completed future will fail tryComplete
      // and we use its result to fulfill lastPromise
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    // the last-completed future will complete the second promise
    first.onComplete(checkAndComplete)
    second.onComplete(checkAndComplete)
    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }

  first(fast, slow).foreach(f => println("FIRST: " + f))
  last(fast, slow).foreach(l => println("LAST: " + l))

  Thread.sleep(1000)

  // 4 - retry util
  def retryUtil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUtil(action, condition)
      }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUtil(action, (x: Int) => x < 10)
    .foreach(result => println(s"settled at $result"))

  Thread.sleep(10000)
}

