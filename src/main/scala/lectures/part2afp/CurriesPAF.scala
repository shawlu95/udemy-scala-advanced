package lectures.part2afp

object CurriesPAF extends App {
  // a curried function
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3)
  println(add3(5))
  println(superAdder(3)(5))

  // a method (curried method)
  def curriedAdder(x: Int)(y: Int): Int = x + y

  // converting method into a function with Int => Int
  // cannot use method in HOF unless transformed into function
  // lift(eta-expansion) converts method into function
  val add4: Int => Int = curriedAdder(4)
  println(add4(6))

  def inc(x: Int) = x + 1
  List(1, 2, 3).map(inc)
  List(1, 2, 3).map(x => inc(x)) // behind-the-scene

  // partial function applications
  // put _ to tell compiler to do eta-expansion
  val add5 = curriedAdder(5) _ // convert to type: Int => Int

  // EXERCISE: how many ways to do HOF
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7a = (x: Int) => simpleAddFunction(7, x)
  val add7b = simpleAddFunction.curried(7)
  val add7c = curriedAddMethod(7) _ // PAF eta-expansion to lift method to a function value
  val add7d = curriedAddMethod(7)(_) // PAF alternative syntax
  val add7e = simpleAddFunction(7, _: Int)
  val add7f = simpleAddMethod(7, _: Int) // turning method into function value

  // use underscore wisely
  // eta-expand: x: String => concat("foo", x, "bar")
  def concat(a: String, b: String, c: String) = a + b + c
  val insertName = concat("Hello I'm ", _: String, ", how are you")
  println(insertName("Shaw"))

  // eta-expand: (x: String, y: String) => concat("Hello!", x, y)
  val moreBlanks = concat("Hello!", _: String, _: String)
  println(moreBlanks("first", "second"))

  /**
   * EXERCISE
   * 1. Process a list of numbers and return string repr with diff formats
   *    Use the %4.2f, %8.6f and %14.12 with curried formatter function
   * 2. difference between functions vs methods
   */
  val numbers = List(Math.E, Math.PI)
  def curriedFormatter(f: String)(a: Double) = f.format(a)

  val format1 = curriedFormatter("%4.2f") _ // lift
  val format2 = curriedFormatter("%8.6f") _ // lift

  println(numbers.map(format1))
  println(numbers.map(format2))
  println(numbers.map(curriedFormatter("%4.2f")))

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1
  def method: Int = 42 // parenthesis-less methods are special
  def parentMethod(): Int = 42

  byName(23) // int: ok
  byName(method) // method: ok
  byName(parentMethod()) // ok with parenthesis, value is passed in
  // byName(parentMethod) not supported after scala3
  // byName(() => 42) // lambda: not ok
  byName((() => 42)()) // ok, value is passed in
  // byName(parentMethod _) // not ok to pass in function

  // byFunction(23) // not ok
  // byFunction(method) // not ok, paramless method, evaluated to value by compiler
  byFunction(parentMethod) // ok, eta-expansion
  byFunction(() => 0) // ok, this is a function
  byFunction(parentMethod _) // ok, eta-expansion

}
