package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {
  /**
   * The classical producer-consumer problem
   * Start with a small container that wraps a single value: [x]
   *
   * A producer threads tries to set the value in the container
   * A consumer tries to extract the value from the container
   *
   * They run in parallel so they don't know each other!
   * How to force consumer to wait for producer to finish the job?
   * In other words, threads are not ordered, how do we enforce order?
   *
   * In practice, we may want to wait for network response
   */

  class SimpleContainer {
    private var value: Int = 0
    def isEmpty: Boolean = value == 0

    // producer calls this
    def set(newValue: Int): Unit = value = newValue

    // consumer colls this
    def get: Int = {
      val result = value
      value  = 0
      result
    }
  }

  // block consumer in a busy loop, wasteful
  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while (container.isEmpty) {
        println("[consumer] still waiting")
      }
      println(s"[consumer] I have consumed: ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println(f"[producer] I have produced $value")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

  // naiveProdCons()

  /**
   * "Synchronized" locks the object's "monitor"
   * any other thread trying to access the object will block
   * at the end of synchronized method, the lock is released
   *
   * Only AnyRefs can have synchronized blocks
   * Primitive types don't have the methods
   *
   * best practices
   * 1. make no assumptions about who gets the lock first
   * 2. keep locking to minimum
   * 3. maintain thread safety at all times in parallel application
   *
   * wait()
   * during synchronized, call wait() to temporarily release the lock
   *
   * notify()
   * during synchronized, call notify() signals ONE sleeping thread to continue
   * (there's also notifyAll() to awaken all waiting threads)
   *
   * consumer calls wait()
   * producer calls notify()
   */
  def smartProdCons(): Unit = {
    val container = new SimpleContainer
    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized{
        container.wait()
      }
      println(s"[consumer] I have consumed ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(2000)
      val value = 42
      container.synchronized{
        println(s"[producer] I'm producing $value")
        container.set(value)
        container.notify() // awake consumer thread
      }
    })

    consumer.start()
    producer.start()
  }

//  [consumer] waiting...
//  [producer] computing...
//  [producer] I'm producing 42
//  [consumer] I have consumed 42
//  smartProdCons()

  /**
   * producer is filling a buffer (3 spots) with values
   * consumer extract NEW values from buffer
   * both are running indefinitely
   * producer and consumer may block each other
   *
   * when buffer is full, producer must block,
   * till consumer extracts some value(s) out of the buffer
   *
   * if buffer is empty, consumer must block,
   * till producer writes some value(s) to buffer
   *
   * producer -> [ ? ? ?] -> consumer
   */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting")
            buffer.wait()
          }
          // woken up by the producer
          // there must be at least one value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer] consumed $x")

          // awake producer, there's empty space available
          buffer.notify()
        }
        // simulate some heavy computation with the extracted value
        // sleep maximum 500ms
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0 // keep track of index
      while (true) {
        buffer.synchronized{
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }
          // woken up by consumer
          // there must be at least one empty space in buffer
          println(f"[producer] producing $i")
          buffer.enqueue(i)

          // awake consumer, there's value available
          buffer.notify()

          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }

  // prodConsLargeBuffer()

  /**
   * multiple producer-consumers acting on the same buffer
   */
  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      while (true) {
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[consumer $id] buffer empty, waiting")
            buffer.wait()
          }
          // woken up by the producer
          // there must be at least one value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed $x")

          // awake producer, there's empty space available
          buffer.notify()
        }
        // simulate some heavy computation with the extracted value
        // sleep maximum 500ms
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0 // keep track of index
      while (true) {
        buffer.synchronized{
          if (buffer.size == capacity) {
            println(s"[producer $id] buffer is full, waiting...")
            buffer.wait()
          }
          // woken up by consumer
          // there must be at least one empty space in buffer
          println(s"[producer $id] producing $i")
          buffer.enqueue(i)

          // awake consumer, there's value available
          buffer.notify()

          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }
}
