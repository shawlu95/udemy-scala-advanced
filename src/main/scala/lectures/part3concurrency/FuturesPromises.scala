package lectures.part3concurrency

import scala.concurrent.Future
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
}

