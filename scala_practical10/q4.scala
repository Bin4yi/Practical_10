object BankApp {

  class Account(val accountNumber: String, var balance: Double) {

    def deposit(amount: Double): Unit = {
      require(amount > 0, "Deposit amount must be positive")
      balance += amount
    }

    def withdraw(amount: Double): Unit = {
      require(amount > 0, "Withdrawal amount must be positive")
      if (amount <= balance) balance -= amount
    }

    def transfer(amount: Double, toAccount: Account): Unit = {
      if (amount <= balance) {
        this.withdraw(amount)
        toAccount.deposit(amount)
      }
    }

    override def toString: String = f"Account($accountNumber, Balance: Rs. $balance%.2f)"
  }

  class Bank(val accounts: List[Account]) {

    def accountsWithNegativeBalances: List[Account] = {
      accounts.filter(_.balance < 0)
    }

    def totalBalance: Double = {
      accounts.map(_.balance).sum
    }

    def applyInterest(): Unit = {
      accounts.foreach { account =>
        if (account.balance > 0) {
          account.deposit(account.balance * 0.05) 
        } else {
          account.balance -= account.balance.abs * 0.1
        }
      }
    }

    def displayAccounts(): Unit = {
      accounts.foreach(println)
    }
  }

  def main(args: Array[String]): Unit = {

    val account1 = new Account("ACC1001", 5000.00)
    val account2 = new Account("ACC2002", -1000.00)
    val account3 = new Account("ACC3003", 3000.00)
    val account4 = new Account("ACC4004", -500.00)

    val bank = new Bank(List(account1, account2, account3, account4))

    println("Accounts with negative balances:")
    bank.accountsWithNegativeBalances.foreach(println)

    val totalBalance = bank.totalBalance
    println(f"\nTotal balance of all accounts: Rs. $totalBalance%.2f")

    bank.applyInterest()

    println("\nAccounts after applying interest:")
    bank.displayAccounts()
  }
}
