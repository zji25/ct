interface Queue<E> {
    fun enqueue(element: E)
    fun dequeue(): E?
    fun validate() {}
}