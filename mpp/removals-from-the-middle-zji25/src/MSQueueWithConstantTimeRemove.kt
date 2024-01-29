@file:Suppress("DuplicatedCode", "FoldInitializerAndIfToElvis")

import java.util.concurrent.atomic.*

class MSQueueWithConstantTimeRemove<E> : QueueWithRemove<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dum = Node<E>(null, null)
        head = AtomicReference(dum)
        tail = AtomicReference(dum)
    }

    override fun enqueue(element: E) {
        while (true) {
            val ct = tail.get()
            val nd = Node(element, ct)
            if (ct.next.compareAndSet(null, nd)) {
                tail.compareAndSet(ct, nd)
                if (ct.extractedOrRemoved) ct.remove()
                return
            } else {
                val nxt = ct.next.get() ?: continue
                tail.compareAndSet(ct, nxt)
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val ch = head.get()
            val chn = ch.next.get() ?: return null
            val result = chn.element
            if (head.compareAndSet(ch, chn)) {
                chn.prev.set(null)
                if (chn.markExtractedOrRemoved()) return result
            }
        }
    }

    override fun remove(element: E): Boolean {
        // Traverse the linked list, searching the specified
        // element. Try to remove the corresponding node if found.
        // DO NOT CHANGE THIS CODE.
        var node = head.get()
        while (true) {
            val next = node.next.get()
            if (next == null) return false
            node = next
            if (node.element == element && node.remove()) return true
        }
    }

    /**
     * This is an internal function for tests.
     * DO NOT CHANGE THIS CODE.
     */
    override fun validate() {
        check(head.get().prev.get() == null) {
            "`head.prev` must be null"
        }
        check(tail.get().next.get() == null) {
            "tail.next must be null"
        }
        var node = head.get()
        while (true) {
            if (node !== head.get() && node !== tail.get()) {
                check(!node.extractedOrRemoved) {
                    "Removed node with element ${node.element} found in the middle of the queue"
                }
            }
            val nodeNext = node.next.get()
            if (nodeNext == null) break
            val nodeNextPrev = nodeNext.prev.get()
            check(nodeNextPrev != null) {
                "The `prev` pointer of node with element ${nodeNext.element} is `null`, while the node is in the middle of the queue"
            }
            check(nodeNextPrev == node) {
                "node.next.prev != node; `node` contains ${node.element}, `node.next` contains ${nodeNext.element}"
            }
            node = nodeNext
        }
    }

    private class Node<E>(
        var element: E?,
        prev: Node<E>?
    ) {
        var next = AtomicReference<Node<E>?>(null)
        val prev = AtomicReference(prev)

        private val _extractedOrRemoved = AtomicBoolean(false)
        val extractedOrRemoved
            get() =
                _extractedOrRemoved.get()

        fun markExtractedOrRemoved(): Boolean =
            _extractedOrRemoved.compareAndSet(false, true)

        fun remove(): Boolean {
            val removed = markExtractedOrRemoved()
            val cn = next.get() ?: return removed
            val cp = prev.get() ?: return removed
            while (true) {
                val tp = cn.prev.get() ?: break
                if (cn.prev.compareAndSet(tp, cp)) {
                    cp.next.set(cn)
                    break
                }
            }
            if (cp.extractedOrRemoved) cp.remove()
            if (cn.extractedOrRemoved) cn.remove()
            return removed
        }
    }
}