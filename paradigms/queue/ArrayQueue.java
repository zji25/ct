package queue;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Predicate;

public class ArrayQueue extends AbstractQueue implements Queue {
    private final int DEFAULT = 5;
    private Object[] elements = new Object[DEFAULT];
    private int start = 0;

    @Override
    protected void enqueueImpl(final Object element) {
        if (isFull()) capacityX2();
        elements[end()] = element;
    }

    @Override
    protected Object elementImpl() {
        return elements[start];
    }

    @Override
    protected void dequeueImpl() {
        elements[start] = null;
        start = (start + 1) % elements.length;
    }

    @Override
    protected void clearImpl() {
        start = 0;
        elements = new Object[DEFAULT];
    }

    @Override
    protected void pushImpl(final Object element) {
        if (isFull()) capacityX2();
        start = start == 0 ? elements.length - 1 : start - 1;
        elements[start] = element;
    }

    @Override
    protected Object peekImpl() {
        return elements[(end() - 1 + elements.length) % elements.length];
    }

    @Override
    protected void removeImpl() {
        elements[(end() - 1 + elements.length) % elements.length] = null;
    }

    @Override
    public int count(final Object element) {
        int result = 0;
        Object[] temp = toZero(size);
        for (int i = 0; i < size; i++) {
            if (temp[i].equals(element)) {
                result++;
            }
        }
        return result;
    }

    @Override
    public String string() {
        return Arrays.toString(toZero(size));
    }

//  p: predicate != null
    public ArrayQueue filter(Predicate<Object> predicate) {
        ArrayQueue result = new ArrayQueue();
        Object[] temp = toZero(size);
        for (int i = 0; i < size; i++) {
            if (predicate.test(temp[i])) result.enqueue(temp[i]);
        }
        return result;
    }
//  q: immutable size, q[0:size-1] & return value = ArrayQueue q1:
//     & 0 <= q1.size <= size
//     & exists F: X -> Y;  X, Y ⊂ (N ∪ 0):
//         for each i in [0:size-1]: predicate.test(q[i]) == true -> exists i' = F(i): q1[i'] = q[i]
//                                   predicate.test(q[i]) == false -> F(i) doesn't exist
//         for each i, j in [0:size-1] & i < j & predicate.test(q[i]) == predicate.test(q[j]) == true:
//         exist i' = F(i), j' = F(j) in [0:q1.size-1]:
//         i' < j' & q1[i'] = q[i] & q1[j'] = q[j]

//  p: function != null
    public ArrayQueue map(Function<Object, Object> function) {
        ArrayQueue result = new ArrayQueue();
        Object[] temp = toZero(size);
        Object applied;
        for (int i = 0; i < size; i++) {
            applied = function.apply(temp[i]);
            if (applied != null) result.enqueue(applied);
        }
        return result;
    }
//  q: immutable size, q[0:size-1] & return value = ArrayQueue q1:
//     & 0 <= q1.size <= size
//     & exists F: X -> Y;  X, Y ⊂ (N ∪ 0):
//         for each i in [0:size-1]: function.apply(q[i]) != null -> exists i' = F(i): q1[i'] = function.apply(q[i])
//                                   function.apply(q[i]) == null -> F(i) doesn't exist
//         for each i, j in [0:size-1] & i < j & function.apply(q[i]) != null & function.apply(q[j]) != null
//         exist i' = F(i), j' = F(j) in [0:q1.size-1]:
//         i' < j' & q1[i'] = function.apply(q[i]) & q1[j'] = function.apply(q[j])

    private boolean isFull() {
//      p: true
        return size == elements.length;
//      q: immutable q[0:size-1], size
//         & return value = (size == elements.length)
    }

    private int end() {
//      p: true
        return (start + size) % elements.length;
//      q: immutable q[0:size-1], size
//        & return value = (start + size) % elements.length
//        & 0 <= return value < elements.length
    }

    private void capacityX2() {
//      p: elements.length >= default & elements.length >= size
        elements = toZero(elements.length * 2);
        start = 0;
//      q: immutable q[0:size-1], size
    }

    private Object[] toZero(int capacity) {
//      p: capacity >= size >= 0
        Object[] temp = new Object[capacity];
        if (capacity == 0) return temp;
        int m = Math.min(elements.length - start, size);
        System.arraycopy(elements, start, temp, 0, m);
        if (end() - 1 < start) {
            System.arraycopy(elements, 0, temp, m, end());
        }
        return temp;
//      q: returns array: array.length = capacity
//        & immutable q[0:size-1], size
//        & for i in [0:size-1] array[i] = q[i]
    }
}
