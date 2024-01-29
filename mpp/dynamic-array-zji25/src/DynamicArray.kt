package mpp.dynamicarray

import kotlinx.atomicfu.*

interface DynamicArray<E> {
    fun get(index: Int): E
    fun put(index: Int, element: E)
    fun pushBack(element: E)
    val size: Int
}

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY, 0))
    override fun get(index: Int): E {
        val cv = core.value
        require(index in 0..<cv.size.value)
        return core.value.array[index].value!!
    }
    override fun put(index: Int, element: E) {
        var cv = core.value
        require(index in 0..<cv.size.value)
        cv.array[index].getAndSet(element) //= element
        while (true) {
            val next = cv.next.value ?: return
            next.array[index].getAndSet(element)
            cv = next
        }
    }
    override fun pushBack(element: E) {
        while (true) {
            val cv = core.value
            val size = cv.size.value
            if (size < cv.capacity) {
                if (cv.array[size].compareAndSet(null, element)) {
                    cv.size.compareAndSet(size, size+1)
                    break
                } else cv.size.compareAndSet(size, size+1)
            } else {
                val cv2 = Core<E>(cv.capacity*2, cv.capacity)
                if (cv.next.compareAndSet(null, cv2)) {
                    for (i in 0 ..< size) {
                        val e = cv.array[i].value ?: continue
                        cv2.array[i].compareAndSet(null, e)
                    }
                    core.compareAndSet(cv, cv2)
                } else {
                    val nxt = cv.next.value ?: continue
                    for (i in 0..< size) {
                        val e = cv.array[i].value ?: continue
                        nxt.array[i].compareAndSet(null, e)
                    }
                    core.compareAndSet(cv, nxt)
                }
            }
        }
    }

    override val size: Int get() = core.value.size.value
}

private class Core<E>(
    val capacity: Int,
    size: Int
) {
    val array = atomicArrayOfNulls<E>(capacity)
    val next: AtomicRef<Core<E>?> = atomic(null)
    val size = atomic(size)
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME