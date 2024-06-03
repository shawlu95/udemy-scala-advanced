package lectures.part3concurrency

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
  smartProdCons()
}
