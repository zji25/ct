import java.util.concurrent.atomic.*

/**
 * @author Yarunina Xenia
 */
class FAABasedQueue<E> : Queue<E> {
    private var head: Segment = Segment(0)
    private var tail: Segment = head
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)

    private fun findSegment(start: Segment, id: Int): Segment {
        var curSegment = start
        while (curSegment.id < id) {
            curSegment.next.compareAndSet(null, Segment(curSegment.id + 1))
            curSegment = curSegment.next.get()!!
        }
        return curSegment
    }

    override fun enqueue(element: E) {
        while (true) {
            val curTail = tail
            val i = enqIdx.getAndIncrement().toInt()
            val s = findSegment(curTail, i / SEGMENT_SIZE)
            tail = s
            if (s.cells.compareAndSet(i % SEGMENT_SIZE, null, element)) return
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (!shouldTryToDequeue()) return null
            val curHead = head
            val i = deqIdx.getAndIncrement().toInt()
            val s = findSegment(curHead, i / SEGMENT_SIZE)
            head = s
            if (s.cells.compareAndSet(i % SEGMENT_SIZE, null, POISONED)) continue
            return s.cells.get(i % SEGMENT_SIZE) as E
        }
    }

    private fun shouldTryToDequeue(): Boolean {
        while (true) {
            val curEnqIdx = enqIdx.get()
            val curDeqIdx = deqIdx.get()
            if (curEnqIdx != enqIdx.get()) continue
            return curDeqIdx < curEnqIdx
        }
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2
private val POISONED = Any()
