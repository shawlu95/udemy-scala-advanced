package lectures.part2afp

object LazyEvaluation extends App {
  // won't crash the program unless x is USED
  lazy val x: Int = throw new RuntimeException

  lazy val y: Int = {
    println("computing y")
    1
  }
  println(y) // print computing y
  println(y) // not print computing y

  def sideEffectCondition: Boolean = {
    println("boolean")
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  // no need to evaluate because simpleCondition = false already
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  // in conjunction with call-by-name
  def byName(n: => Int): Int = n + n + n + 1
  def retrieve: Int = {
    // side effect or long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }
  // evaluated 3 times! print "waiting" 3 times
  println(byName(retrieve))

  // technique: call-by-need
  def byNeed(n: => Int) = {
    lazy val t = n
    t + t + t + 1
  }
  println(byNeed(retrieve))

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30) // eta-expansion
  val gt20 = lt30.filter(greaterThan20)
  println(lt30)
  println(gt20)

  // check e1 < 30, e1 > 20, e2 < 30 ...
  val lt30Lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30Lazy.withFilter(greaterThan20)
  println(gt20Lazy) // not evaluated
  gt20Lazy.foreach(println)

  // if-guard uses lazy val in comprehension
  for {
    a <- List(1, 2, 3) if a % 2 == 0
  } yield a + 1
  List(1, 2, 3).withFilter(_ % 2 == 0).map(_ + 1)
}
