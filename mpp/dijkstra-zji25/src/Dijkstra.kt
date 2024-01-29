package dijkstra

import kotlinx.atomicfu.atomic
import java.util.*
import java.util.concurrent.Phaser
import kotlin.Comparator
import kotlin.concurrent.thread
import kotlin.random.Random

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    start.distance = 0
    val cq = MultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    cq.add(start)
    val onFinish = Phaser(workers + 1)
    repeat(workers) {
        thread {
            while (true) {
                val u = cq.delete() ?: if (cq.active.value == 0) break else continue
                for (v in u.outgoingEdges) if (v.updateDistIfLower(u.distance + v.weight)) cq.add(v.to)
                cq.active.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

private class MultiQueue(workers: Int, val comparator: Comparator<Node>) {
    val active = atomic(0)
    val queues = MutableList(workers) { PriorityQueue(comparator) }
    fun add(node: Node) {
        val q = queues[Random.nextInt(queues.size)]
        active.incrementAndGet()
        synchronized(q) { q.add(node) }
    }
    fun delete(): Node? {
        val i = Random.nextInt(queues.size)
        val q1 = queues[i]
        val q2 = queues[(i + 1) % queues.size]
        synchronized(q1) { synchronized(q2) {
            if (q1.peek() != null) {
                return if (q2.peek() == null || comparator.compare(q1.peek(), q2.peek()) < 0) q1.poll()
                else q2.poll()
            }
            else return q2.poll()
        }}
    }
}
