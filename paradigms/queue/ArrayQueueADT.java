package queue;

import java.util.Arrays;

// model: q[0] ... q[size-1] (current queue size)
// invariant: q != null
//      & size >= 0
//      & for i in [0:size-1] q[i] != null
//      & elements.length >= size
//      & 0 <= start < elements.length
// let immutable a[x:y] be for i in [x:y] a'[i] = a[i]
//     immutable a[x:y]+[z:w] be for i in [x:y] a'[i] = a[i] & for j in [z:w] a'[j] = a[j] if 0 <= i or j < a.length
//     immutable x be for x' = x
// let count(q, element) be = |S| where S is a set of all i: [0 <= i < size] & [q[i] = element]

public class ArrayQueueADT {
    private static final int DEFAULT = 5;
    private Object[] elements = new Object[DEFAULT];
    private int start = 0, size = 0;
//  default parameters: start = 0; size = 0; q.length = 0; elements.length = default

    public static void enqueue(ArrayQueueADT q, final Object element) {
        assert q != null;
        assert element != null;
//      p: q != null & element != null
        if (isFull(q)) capacityX2(q);
//        p0: size == elements.length
//        q0: elements'.length = elements.length * 2
//           & immutable size & start' = 0
        q.elements[end(q)] = element;
        q.size++;
//      q: q[size] = element & immutable q[0:size-1]
//         & size' = size + 1
//         & start' = [0 if was full] [else start]
//         & elements' = if (was not full)
//                          elements[0:end-1] + [element] + [end+1:elements.length-1]
//                       else if (was full & end-1 < start)
//                          elements[start:elements.length-1] + [0:end-1] + [element] + [null] * (elements.length-1)
//                       else
//                          elements[start:elements.length-1 (== end-1)] + [element] + [null] * (elements.length-1)
    }

    public static Object element(ArrayQueueADT q) {
        assert q != null;
        assert q.size != 0;
//      p: q != null & size > 0
        return q.elements[q.start];
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
//         & return value = q[0] != null
    }

    public static Object dequeue(ArrayQueueADT q) {
        assert q != null;
        assert q.size != 0;
//      p: q != null & size > 0
        Object res = q.elements[q.start];
        q.elements[q.start] = null;
        q.start = (q.start + 1) % q.elements.length;
        q.size--;
        return res;
//      q: immutable q[1:size-1] & immutable elements[0:start-1]+[start+1:elements.length-1]
//         & elements[start] = null
//         & size' = size - 1 & start' = (start + 1) % elements.length |->
//           q' = q[1:size-1] & elements' = elements[0:start-1] + [null] + elements[start+1:elements.length-1]
//         & return value = q[0] != null
    }

    public static int size(ArrayQueueADT q) {
        assert q != null;
//      p: q != null
        return q.size;
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
//         & return value = size
    }

    public static boolean isEmpty(ArrayQueueADT q) {
        assert q != null;
//      p: q != null
        return q.size == 0;
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
//         & return value = (size == 0)
    }

    private static boolean isFull(ArrayQueueADT q) {
        assert q != null;
//      p: q != null
        return q.size == q.elements.length;
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
//         & return value = (size == elements.length)
    }

    public static void clear(ArrayQueueADT q) {
        assert q != null;
//      p: q != null
        q.elements = new Object[DEFAULT];
        q.start = q.size = 0;
//      q: size' = 0 & start' = 0
//         & q' = [] & elements'.length = DEFAULT
    }

    public static void push(ArrayQueueADT q, final Object element) {
        assert q != null;
        assert element != null;
//      p: q != null & element != null
        if (isFull(q)) capacityX2(q);
//        p0: size == elements.length
//        q0: elements'.length = elements.length * 2
//           & immutable size & start' = 0
        q.start = q.start == 0 ? q.elements.length - 1 : q.start - 1;
        q.elements[q.start] = element;
        q.size++;
//      q: start' = 0 (if was full) (else start)
//         & start'' = [elements.length - 1 if start' == 0] [else start' - 1]
//         & elements' = if (was not full)
//                          elements[0:start-2] + [element] + elements[start:elements.length-1]
//                       else
//                          elements[0:elements.length-1] + [null] * (elements.length-2) + [element]
    }

    public static Object peek(ArrayQueueADT q) {
        assert q != null;
        assert q.size != 0;
//      p: q != null & size > 0
        return q.elements[(end(q) - 1 + q.elements.length) % q.elements.length];
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
//         & return value = elements[(end() - 1 + elements.length) % elements.length] = q[size-1] != null
    }

    public static Object remove(ArrayQueueADT q) {
        assert q != null;
        assert q.size != 0;
//      p: q != null & size > 0
        Object result = q.elements[(end(q) - 1 + q.elements.length) % q.elements.length];
        q.elements[(end(q) - 1 + q.elements.length) % q.elements.length] = null;
        q.size--;
        return result;
//      q: immutable q[0:size-2] & immutable elements[0:end() - 2] + [end:elements.length]
//         & elements'[(end() - 1 + elements.length) % elements.length] = null
//         & size' = size - 1 & immutable start
//         & return value = elements[end() - 1] != null
    }

    public static int count(ArrayQueueADT q, final Object element) {
        assert q != null;
//      p: q != null
        int result = 0;
        Object[] temp = toZero(q, q.size);
//        q0: temp = q[0:size-1]
//           & immutable q[0:size-1] & immutable elements[0:elements.length-1]
//           & immutable start, size
        for (int i = 0; i < q.size; i++) {
            if (temp[i].equals(element)) {
                result++;
            }
        }
        return result;
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable start, size
//         & return value = count(q, element) >= 0
    }

    private static int end(ArrayQueueADT q) {
        assert q != null;
//      p: q != null
        return (q.start + q.size) % q.elements.length;
//      q: immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
//         & return value = (start + size) % elements.length = q.length - 1
//         & 0 <= return value < elements.length
    }

    private static void capacityX2(ArrayQueueADT q) {
        assert q != null;
//      p: q != null & elements.length >= default & elements.length >= size
        q.elements = toZero(q, q.elements.length * 2);
        q.start = 0;
//      q: immutable q[0:size-1]
//         & elements'.length = elements.length * 2
//         & for i in [0:size-1] elements'[i] = q[i]
//         & start' = 0 & immutable size
    }

    private static Object[] toZero(ArrayQueueADT q, int capacity) {
        assert q != null;
        assert capacity >= q.size; //?
//      p: q != null & capacity >= size >= 0
        Object[] temp = new Object[capacity];

//      p0: capacity == 0
        if (capacity == 0) return temp;
//      q0: returns array: array.length = 0

//      p1: capacity > 0, size > 0
        int m = Math.min(q.elements.length - q.start, q.size);
        System.arraycopy(q.elements, q.start, temp, 0, m);
//      q1: copies elements from start [to end if start < end]
//         [else to elements.length] to temp starting index 0
//         & immutable elements[0:elements.length-1] & immutable q[0:size-1]
//         & immutable size, start
        if (end(q) - 1 < q.start) {
//          p2: size > 0 & end - 1 < start
            System.arraycopy(q.elements, 0, temp, m, end(q));
//          q2: copies elements from 0 to end
//             & immutable elements[0:elements.length-1] & immutable q[0:size-1]
//             & immutable size, start
        }
        return temp;
//      q: returns array: array.length = capacity
//        & immutable q[0:size-1] & immutable elements[0:elements.length-1]
//        & immutable size, start
//        & for i in [0:size-1] array[i] = q[i]
    }

    public static String string(ArrayQueueADT q) {
        assert q != null;
//      p: q != null
        return Arrays.toString(toZero(q, q.size));
//      q: returns string of array's elements: array.length = size
//         & for i in [0:size-1] array[i] = q[i]
//         & immutable q[0:size-1] & immutable elements[0:elements.length-1]
//         & immutable size, start
    }
}
