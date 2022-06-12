package exercises

import scala.annotation.tailrec

/** invariant for now */
trait MySet[A] extends (A => Boolean ) {
  def apply(elem: A): Boolean = contains(elem)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /**
   * EXERCISE
   * - remove element
   * - intersection with another set
   * - difference with another set
   */
  def -(elem: A): MySet[A]
  def &(another: MySet[A]): MySet[A]
  def --(another: MySet[A]): MySet[A]
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def map[B](f: A => B): MySet[B] = EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  def -(elem: A): MySet[A] = this
  def &(another: MySet[A]) = this
  def --(another: MySet[A]) = this
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

// describe all elements of type A which satisfies a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  def contains(elem: A): Boolean = property(elem)

  // {x in A | property(x) } + element = { x in A | property(x) || x == element}
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  // {x in A | property(x) } ++ set = { x in A | property(x) || set.contains(x)}
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  // modulo %3 => {0, 1, 2}, infinite becomes finite
  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))
  def foreach(f: A => Unit): Unit = politelyFail

  def -(elem: A): MySet[A] = filter(x => x != elem)
  def &(another: MySet[A]): MySet[A] = filter(another)
  def --(another: MySet[A]): MySet[A] = filter(!another)
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("really deep hole!")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    head == elem || tail.contains(elem)
  def +(elem: A): MySet[A] =
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)

  /**
   * concatenate set
   * [1 2 3] ++ [4 5] =
   * [2 3] ++ [4 5] + 1 =
   * [3] ++ [4 5] + 1 + 2 =
   * [] ++ [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
   */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  // intersecting is same as filtering
  def &(another: MySet[A]): MySet[A] = filter(another)

  def --(another: MySet[A]): MySet[A] = filter(x => !another(x))

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet {
  /**
   * val s = MySet(1, 2, 3)
   * = buildSet(seq(1, 2, 3), [])
   * = buildSet(seq(2, 3), [] + 1)
   * = buildSet(seq(3), [1] + 2)
   * = buildSet(seq(), [1, 2] + 3)
   * = [1, 2, 3]
   */
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc:  MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  val s2 = s + 5 ++ MySet(6, 7)
  s2.foreach(println)
  s.flatMap(x => MySet(x, -x)).foreach(println)
  s.filter(_ % 2 == 0).foreach(println)

  // test property-basedset
  val negative = !s
  println(negative(2)) // false
  println(negative(5)) // true

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5)) // false

  val add5 = negativeEven + 5
  println(add5(5)) // true
}