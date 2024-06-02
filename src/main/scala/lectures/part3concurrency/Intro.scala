package lectures.part3concurrency

import java.util.concurrent.Executors

object Intro extends App {
  // jave has runnable interface, which we treat as trait in scala
  /*
    interface Runnable {
      public void run()
  }
  */

  // JVM threads
  // thread is an instance of a class
  val runnable = new Runnable {
    override def run(): Unit = println("running in parallel")
  }
  val aThread = new Thread(runnable)

  // distinction: thread instance is an object we operate on
  // JVM thread: the actual place where the parallel code runs

  // create a JVM thread, which run on top of OS thread
  // we call start() on instance, not run()!
  aThread.start()

  // this doesn't do anything in parallel
  runnable.run()

  // blocks until aThread finishes running
  aThread.join()

  val threadHello = new Thread(() => (1 to 5).foreach(x => println("Hello" + x)))
  val threadBye = new Thread(() => (1 to 5).foreach(x => println("Bye" + x)))

  // different runs in multi-thread environment produces different results
  // try rerun and you will see different order
  threadHello.start()
  threadBye.start()

  // threads are expensive to start and kill, so we try to reuse them, with executors and pool
  val pool = Executors.newFixedThreadPool(10);
  pool.execute(() => println("something in the thread pool"))
  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })
  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done after two seconds")
  })
  // should wait for 2 seconds in total
  // after 1 sec, see "done after 1 second" and "almost done" at the same time

  // shutdown all threads
  pool.shutdown()

  // will throw exception
  // pool.execute(() => println("should not appear"))

  // in the main thread, will interrupt sleep from running thread
  // pool.shutdownNow()

  // will return true even if threads are still running
  // shutdown means no longer accepting runnables
  println(pool.isShutdown)
}
