const val MAX_AMOUNT = 1000000000000000L

interface Bank {
    val numberOfAccounts: Int
    fun getAmount(index: Int): Long
    val totalAmount: Long
    fun deposit(index: Int, amount: Long): Long
    fun withdraw(index: Int, amount: Long): Long
    fun transfer(fromIndex: Int, toIndex: Int, amount: Long)
}