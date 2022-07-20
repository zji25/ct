package queue;

// let immutable a[x:y] be for i in [x:y] a'[i] = a[i]
//     immutable x be for x' = x

import java.util.function.Function;
import java.util.function.Predicate;

public interface Queue {
//  model: q[0] ... q[size-1] (current queue size)
//  invariant: size >= 0 & for i in [0:size-1] q[i] != null

//  p: element != null
    void enqueue(final Object element);
//  q: size' = size + 1 & q' = immutable q[0:size-1] + [element]

//  p: size > 0
    Object element();
//  q: immutable q[0:size-1], size
//     & return value = q[0] != null

//  p: size > 0
    Object dequeue();
//  q: size' = size - 1 & q' = immutable q[1:size-1]
//     & return value = q[0] != null

//  p: true
    int size();
//  q: immutable q[0:size-1], size
//     & return value = size

//  p: true
    boolean isEmpty();
//  q: immutable q[0:size-1], size
//     & return value = (size == 0)

//  p: true
    void clear();
//  q: q = [] & size' = 0

//  p: element != null
    void push(final Object element);
//  q: size' = size + 1 & q' = immutable q[0:size-1] + [element]

//  p: size > 0
    Object peek();
//  q: immutable q[0:size-1], size
//     & return value = q[size-1] != null

//  p: size > 0
    Object remove();
//  q: q' = immutable q[0:size-2] & size' = size - 1
//     & return value = q[size-1] != null

//  p: true
    int count(final Object element);
//  q: immutable q[0:size-1], size
//     & return value = |A| [where A is a set of all i: i ⊂ Z & [0 <= i < size] & [q[i] = element]] >= 0

//  p: true
    String string();
//  q: immutable q[0:size-1], size
//     & return value = '[' + string of q[0:size-1]'s elements (sep=", ") + ']'

//  p: predicate != null
    Queue filter(final Predicate<Object> predicate);
//  q: immutable size, q[0:size-1] & return value = q1:
//     q1.getClass() = q.getClass()
//     & 0 <= q1.size <= size
//     & exists F: X -> Y;  X, Y ⊂ (N ∪ 0):
//         for each i in [0:size-1]: predicate.test(q[i]) == true -> exists i' = F(i): q1[i'] = q[i]
//                                   predicate.test(q[i]) == false -> F(i) doesn't exist
//         for each i, j in [0:size-1] & i < j & predicate.test(q[i]) == predicate.test(q[j]) == true:
//         exist i' = F(i), j' = F(j) in [0:q1.size-1]:
//         i' < j' & q1[i'] = q[i] & q1[j'] = q[j]

//  p: function != null
    Queue map(final Function<Object, Object> function);
//  q: immutable size, q[0:size-1] & return value = q1:
//     q1.getClass() = q.getClass()
//     & 0 <= q1.size <= size
//     & exists F: X -> Y;  X, Y ⊂ (N ∪ 0):
//         for each i in [0:size-1]: function.apply(q[i]) != null -> exists i' = F(i): q1[i'] = function.apply(q[i])
//                                   function.apply(q[i]) == null -> F(i) doesn't exist
//         for each i, j in [0:size-1] & i < j & function.apply(q[i]) != null & function.apply(q[j]) != null
//         exist i' = F(i), j' = F(j) in [0:q1.size-1]:
//         i' < j' & q1[i'] = function.apply(q[i]) & q1[j'] = function.apply(q[j])
}
