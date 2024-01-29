/**
 * Bank implementation.
 *
 * @author Yarunina Xenia
 */
import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
        accounts[index].lock.withLock {
            return accounts[index].amount
        }
    }

    override val totalAmount: Long
        get() {
            accounts.forEach { it.lock.lock() }
            val sum = accounts.sumOf {
                    account ->
                    account.amount
            }
            accounts.forEach { it.lock.unlock() }
            return sum
        }

    override fun deposit(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        account.lock.withLock {
            check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            account.amount += amount
            return account.amount
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        require(amount > 0) { "Invalid amount: $amount" }
        val account = accounts[index]
        account.lock.withLock {
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            return account.amount
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        require(amount > 0) { "Invalid amount: $amount" }
        require(fromIndex != toIndex) { "fromIndex == toIndex" }
        val from = accounts[fromIndex]
        val to = accounts[toIndex]
        if (fromIndex < toIndex) {
            from.lock.lock()
            to.lock.lock()
        } else {
            to.lock.lock()
            from.lock.lock()
        }
        try {
            check(amount <= from.amount) { "Underflow" }
            check(!(amount > Bank.MAX_AMOUNT || to.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            from.amount -= amount
            to.amount += amount
        } finally {
            from.lock.unlock()
            to.lock.unlock()
        }
    }

    class Account {
        var amount: Long = 0
        var lock: ReentrantLock = ReentrantLock()
    }
}