import java.util.concurrent.atomic.*

interface Environment {
    fun park()
    fun unpark(thread: Thread)
}

var <T> AtomicReference<T>.value: T
    get() = get()
    set(value) = set(value)
