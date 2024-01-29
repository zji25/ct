import java.util.concurrent.*
import java.util.concurrent.atomic.*

/**
 * @author Yarunina Xenia
 */

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>()
    private val combinerLock = AtomicBoolean(false)
    private val tasksForCombiner = AtomicReferenceArray<Any?>(TASKS_FOR_COMBINER_SIZE)

    override fun enqueue(element: E) {
        val cellIndex = randomCellIndex()
        while (true) {
            if (combinerLock.compareAndSet(false, true)) return enqueueLocked(element)
            if (tasksForCombiner.compareAndSet(cellIndex, null, element)) break
        }
        while (true) {
            val result = tasksForCombiner.get(cellIndex)
            if (result is Result<*>) if (tasksForCombiner.compareAndSet(cellIndex, result, null)) return
            if (combinerLock.compareAndSet(false, true)) {
                if (tasksForCombiner.compareAndSet(cellIndex, element, null)) return enqueueLocked(element)
                else combinerLock.set(false)
            }
        }
    }
    override fun dequeue(): E? {
        val cellIndex = randomCellIndex()
        while (true) {
            if (combinerLock.compareAndSet(false, true)) return dequeueLocked()
            if (tasksForCombiner.compareAndSet(cellIndex, null, Dequeue)) break
        }
        while (true) {
            val result = tasksForCombiner.get(cellIndex) as? Result<*>
            if (result != null) {
                if (tasksForCombiner.compareAndSet(cellIndex, result, null)) {
                    @Suppress("UNCHECKED_CAST")
                    return result.value as E?
                }
            }
            if (combinerLock.compareAndSet(false, true)) {
                if (tasksForCombiner.compareAndSet(cellIndex, Dequeue, null)) return dequeueLocked()
                else combinerLock.set(false)
            }
        }
    }

    private fun enqueueLocked(element: E) {
        queue.addLast(element)
        help()
        combinerLock.set(false)
    }
    private fun dequeueLocked(): E? {
        val element = queue.removeFirstOrNull()
        help()
        combinerLock.set(false)
        return element
    }
    private fun help() {
        for (i in 0 until TASKS_FOR_COMBINER_SIZE) {
            val task = tasksForCombiner.get(i)
            if (task == Dequeue) tasksForCombiner.set(i, Result(queue.removeFirstOrNull()))
            else {
                val result = task as? Result<*>
                if (result != null) continue
                @Suppress("UNCHECKED_CAST")
                val element = task as? E
                if (element != null) {
                    queue.addLast(element)
                    tasksForCombiner.set(i, Result(null))
                }
            }
        }
    }
    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(tasksForCombiner.length())

    private object Dequeue
    private class Result<V>(
        val value: V
    )
}

