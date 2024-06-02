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
    demoBankingProblem()
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
}
