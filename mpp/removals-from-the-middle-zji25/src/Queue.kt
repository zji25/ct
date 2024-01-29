interface Queue<E> {
    fun enqueue(element: E)
    fun dequeue(): E?
    fun validate() {}
}

interface QueueWithRemove<E> : Queue<E> {
    fun remove(element: E): Boolean
}
