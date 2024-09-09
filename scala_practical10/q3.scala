object BankingApp {

  class Account(val accountNumber: String, var balance: Double) {

    def deposit(amount: Double, printMessage: Boolean = true): Unit = {
      require(amount > 0, "Deposit amount must be positive")
      balance += amount
      if (printMessage) {
        println(f"Deposited Rs. $amount%.2f to account $accountNumber. New balance: Rs. $balance%.2f")
      }
    }

    def withdraw(amount: Double, printMessage: Boolean = true): Unit = {
      require(amount > 0, "Withdrawal amount must be positive")
      if (amount <= balance) {
        balance -= amount
        if (printMessage) {
          println(f"Withdrew Rs. $amount%.2f from account $accountNumber. New balance: Rs. $balance%.2f")
        }
      } else {
        println(f"Insufficient funds in account $accountNumber. Current balance: Rs. $balance%.2f")
      }
    }

    def transfer(amount: Double, toAccount: Account): Unit = {
      require(amount > 0, "Transfer amount must be positive")
      if (amount <= balance) {
        this.withdraw(amount, printMessage = false)
        toAccount.deposit(amount, printMessage = false)
        println(f"Transferred Rs. $amount%.2f from account $accountNumber to account ${toAccount.accountNumber}")
      } else {
        println(f"Transfer failed: insufficient funds in account $accountNumber. Current balance: Rs. $balance%.2f")
      }
    }

    override def toString: String = f"Account($accountNumber, Balance: Rs. $balance%.2f)"
  }

  def main(args: Array[String]): Unit = {
    val account1 = new Account("ACC1001", 5000.00)
    val account2 = new Account("ACC2002", 3000.00)

    println(account1)
    println(account2)

    account1.deposit(1500.00)
    account1.withdraw(2000.00)
    account1.transfer(1000.00, account2)

    println(account1)
    println(account2)
  }
}
