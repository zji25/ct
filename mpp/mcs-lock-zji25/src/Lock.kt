interface Lock<N> {
    fun lock(): N
    fun unlock(node: N)
}

inline fun <N> Lock<N>.withLock(block: () -> Unit) {
    val node = lock()
    block()
    unlock(node)
}