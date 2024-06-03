package lectures.part3concurrency

object JVMConcurrencyProblems {
  def runInParallel(): Unit = {
    // variable is the root of all evil in distributed app
    var x = 0
    val thread1 = new Thread(() => {
      // modify the thread variable
      x = 1
    })

    val thread2 = new Thread(() => {
      // modify the thread variable
      x = 2
    })

    thread1.start()
    thread2.start()
    Thread.sleep(100)
    // not deterministic, race condition
    println(x)
  }

  case class BankAccount(var amount: Int)

  def buy(account: BankAccount, thing: String, price: Int): Unit = {
    /*
      involves 3 steps
      1. read old value
      2. compute
      3. write new value
    */
    account.amount -= price
  }

  def buySafe(account: BankAccount, thing: String, price: Int): Unit = {
    // does not allow multiple threads to run the critical section at the same time
    account.synchronized{
      account.amount -= price // critical section, subject to race condition
    }
  }

  def main(args: Array[String]): Unit = {
    // runInParallel()
    // demoBankingProblem()

    inceptionThreads(50).start()
  }

  def demoBankingProblem(): Unit = {
    (1 to 1000000).foreach{ _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buy(account, "shoes", 3000))
      val thread2 = new Thread(() => buy(account, "iphone", 4000))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if (account.amount != 43000) println(s"the bank account balance is inconsistent: ${account.amount}")
    }
  }

  /**
   * Exercise
   * 1. create "inception threads"
   *  thread1, print("hello from thread 1")
   *    creates thread 2, print("hello from thread 2")
   *      creates thread 3, print("hello from thread 3")
   * print all messages in REVERSE order
   *
   * 2. what's the max/min value of x?
   *
   * 3. sleep fallacy
   */
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread =
    new Thread(() => {
      if (i < maxThreads) {
        val newThread = inceptionThreads(maxThreads, i + 1)
        newThread.start()
        // wait till the thread finishes
        newThread.join()
      }
      println(s"Hello from thread $i")
    })

  // we always get x = 1, because all threads read value 0, and increase it to 1
  def minMaxX(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }

  /**
   * Almost always get "Scala is awesome"
   * is it always guaranteed? No
   *
   * Obnoxious situation
   * Some JVM, sleep "yields execution"
   */
  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread() {
      Thread.sleep(1000) // one second later, change message
      message = "Scala is awesome"
    }
    // main thread
    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(1001)

    // to guarantee, block thread that does the work
    awesomeThread.join()

    println(message)
  }
}
