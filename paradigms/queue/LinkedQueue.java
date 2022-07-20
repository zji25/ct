package queue;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Predicate;

public class LinkedQueue extends AbstractQueue implements Queue {
    private class Node {
        private final Object element;
        private Node prev, next;
        public Node(Object element, Node prev, Node next) {
            assert element != null;
            this.element = element;
            this.prev = prev;
            this.next = next;
        }
    }

    private Node start, end;

    @Override
    protected void enqueueImpl(final Object element) {
        Node temp = end;
        end = new Node(element, temp, null);
        if (isEmpty()) start = end; else temp.next = end;
    }

    @Override
    protected Object elementImpl() {
        return start.element;
    }

    @Override
    protected void dequeueImpl() {
        start = start.next;
    }

    @Override
    protected void clearImpl() {
        start = end = null;
    }

    @Override
    protected void pushImpl(final Object element) {
        Node temp = start;
        start = new Node(element, null, temp);
        if (isEmpty()) end = start; else temp.prev = start;
    }

    @Override
    protected Object peekImpl() {
        return end.element;
    }

    @Override
    protected void removeImpl() {
        end = end.prev;
    }

    @Override
    public int count(final Object element) {
        int result = 0;
        if (isEmpty()) return result;
        for (Node temp = start; temp != null; temp = temp.next) {
            if (temp.element.equals(element)) result++;
        }
        return result;
    }

    @Override
    public String string() {
        return Arrays.toString(toArray());
    }

//  p: predicate != null
    public LinkedQueue filter(Predicate<Object> predicate) {
        LinkedQueue result = new LinkedQueue();
        for (Node temp = start; temp != null; temp = temp.next) {
            if (predicate.test(temp.element)) result.enqueue(temp.element);
        }
        return result;
    }
//  q: immutable size, q[0:size-1] & return value = LinkedQueue q1:
//     & 0 <= q1.size <= size
//     & exists F: X -> Y;  X, Y ⊂ (N ∪ 0):
//         for each i in [0:size-1]: predicate.test(q[i]) == true -> exists i' = F(i): q1[i'] = q[i]
//                                   predicate.test(q[i]) == false -> F(i) doesn't exist
//         for each i, j in [0:size-1] & i < j & predicate.test(q[i]) == predicate.test(q[j]) == true:
//         exist i' = F(i), j' = F(j) in [0:q1.size-1]:
//         i' < j' & q1[i'] = q[i] & q1[j'] = q[j]


//  p: function != null
    public LinkedQueue map(Function<Object, Object> function) {
        LinkedQueue result = new LinkedQueue();
        for (Node temp = start; temp != null; temp = temp.next) {
            result.enqueue(function.apply(temp.element));
        }
        return result;
    }
//  q: immutable size, q[0:size-1] & return value = LinkedQueue q1:
//     & 0 <= q1.size <= size
//     & exists F: X -> Y;  X, Y ⊂ (N ∪ 0):
//         for each i in [0:size-1]: function.apply(q[i]) != null -> exists i' = F(i): q1[i'] = function.apply(q[i])
//                                   function.apply(q[i]) == null -> F(i) doesn't exist
//         for each i, j in [0:size-1] & i < j & function.apply(q[i]) != null & function.apply(q[j]) != null
//         exist i' = F(i), j' = F(j) in [0:q1.size-1]:
//         i' < j' & q1[i'] = function.apply(q[i]) & q1[j'] = function.apply(q[j])

//  p: true
    private Object[] toArray() {
        Object[] result = new Object[size];
        Node temp = start;
        for (int i = 0; temp != null & i < size; temp = temp.next, i++) {
            result[i] = temp.element;
        }
        return result;
    }
//  q: returns array: array.length = size
//       & immutable q[0:size-1], size
//       & for i in [0:size-1] array[i] = q[i]
}
