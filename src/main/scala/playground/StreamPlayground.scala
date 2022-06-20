package playground

import scala.annotation.tailrec

/**
 * Exercise: implement a lazily evaluated, singly linked stream of elements
 */
abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](elem: B): MyStream[B] // prepend
  def ++[B >: A](another: MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()

  /**
   * an auxillary method to help takeAsList
   * [1 2 3].toList([]) =
   * [2 3].toList([1]) =
   * [3].toList([2 1]) =
   * [].toList([3 2 1]) = [1 2 3]
   */
  @tailrec // type-covariant, B is super type of A
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc
    else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def #::[B >: Nothing](elem: B): MyStream[B] = new Cons(elem, this) // prepend
  def ++[B >: Nothing](another: MyStream[B]): MyStream[B] = another

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this // no element to map/flatmap
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this
}

// call-by-name forces lazy evaluation
class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  // note: val for head, lazy val for head
  override val head: A = hd
  override lazy val tail: MyStream[A] = tl // call by need

  def #::[B >: A](elem: B): MyStream[B] = new Cons(elem, this) // prepend
  def ++[B >: A](another: MyStream[B]): MyStream[B] = new Cons(head, tail ++ another)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  // looks like call-by-name, but in fact call-by-need
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))

  // ++ preserves lazy eval
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate)) // call-by-name, lazy eval
    else tail.filter(predicate) // force eval the first elem of tail

  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons(start, MyStream.from(generator(start))(generator))
}

object StreamPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // same as naturals.#::(0)
  println(startFrom0.head)
  println(startFrom0.tail.head)

  startFrom0.take(10).foreach(println)

  println(startFrom0.map(_ * 2).take(4).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(3).toList())

}
