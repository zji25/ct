package queue;

// copies contract's (p & q)'s listed in Queue interface

// let immutable a[x:y] be for i in [x:y] a'[i] = a[i]
//     immutable x be for x' = x

import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size;

    public void enqueue(final Object element) {
        assert element != null;
        enqueueImpl(element);
        size++;
    }

    public Object element() {
        assert size != 0;
        return elementImpl();
    }

    public Object dequeue() {
        assert size != 0;
        Object res = elementImpl();
        dequeueImpl();
        size--;
        return res;
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() { // common
        return size == 0;
    }

    public void clear() {
        clearImpl();
        size = 0;
    }

    public void push(final Object element) {
        assert element != null;
        pushImpl(element);
        size++;
    }

    public Object peek() {
        assert size != 0;
        return peekImpl();
    }

    public Object remove() {
        assert size != 0;
        Object result = peekImpl();
        removeImpl();
        size--;
        return result;
    }

    public abstract int count(final Object element);
    public abstract String string();
    public abstract AbstractQueue filter(final Predicate<Object> predicate);
    public abstract AbstractQueue map(final Function<Object, Object> function);

//  p: element != null
    protected abstract void enqueueImpl(final Object element);
//  q: q' = immutable q[0:size-1] + [element], immutable size

//  p: size > 0
    protected abstract Object elementImpl();
//  q: immutable q[0:size-1], size & return value = q[0] != null

//  p: size > 0 & return value for dequeue() = q[0] saved in a variable
    protected abstract void dequeueImpl();
//  q: q' = immutable q[1:size-1] & immutable size

//  p: true
    protected abstract void clearImpl();
//  q: q' = []

//  p: element != null
    protected abstract void pushImpl(final Object element);
//  q: immutable size & q' = [element] + immutable q[0:size-1]

//  p: size > 0
    protected abstract Object peekImpl();
//  q: immutable q[0:size-1], size & return value = q[size-1] != null

//  p: size > 0 & return value for remove() = q[size-1] saved in a variable
    protected abstract void removeImpl();
//  q: q' = immutable q[0:size-2], immutable size
}
