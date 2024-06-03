package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success}

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
//    promise.success(42)
//    println("[producer] done")

    promise.failure(new RuntimeException("exception"))
    println("[producer] failed")
  })

  producer.start()
  Thread.sleep(1000)
}

