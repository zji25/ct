package info.kgeorgiy.ja.yarunina.arrayset;

import java.util.*;

@SuppressWarnings("unused")
public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {
    private final List<E> list;
    private final Comparator<E> comparator;

    public ArraySet() {
        this.list = List.of();
        this.comparator = null;
    }

    public ArraySet(final List<E> list, final Comparator<E> comparator) {
        this.list = list;
        this.comparator = comparator;
    }

    public ArraySet(final Collection<E> collection, final Comparator<E> comparator) {
        final TreeSet<E> treeSet = new TreeSet<>(comparator);
        treeSet.addAll(collection);
        this.list = new ArrayList<>(treeSet);
        this.comparator = comparator;
    }

    public ArraySet(final Collection<E> collection) {
        this(collection, null);
    }

    private ArraySet(final ArraySet<E> set, final int from, final int to) {
        this.list = set.list.subList(from, to);
        this.comparator = set.comparator;
    }

    private ArraySet(final ReversedList<E> reversedList, final Comparator<E> comparator) {
        this.list = reversedList;
        this.comparator = comparator;
    }

    @Override
    public E lower(final E e) {
        return getElement(convertIndex(e, -1, 1));
    }

    @Override
    public E floor(final E e) {
        return getElement(convertIndex(e, 0, 1));
    }

    @Override
    public E ceiling(final E e) {
        return getElement(convertIndex(e, 0, 0));
    }

    @Override
    public E higher(final E e) {
        return getElement(convertIndex(e, 1, 0));
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException("pollFirst operation isn't supported for ArraySet");
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException("pollLast operation isn't supported for ArraySet");
    }

    @Override
    public Iterator<E> iterator() {
        return Collections.unmodifiableList(this.list).iterator();
    }

    @Override
    public int size() {
        return this.list.size();
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(final Object o) {
        return binarySearch((E) o) >= 0;
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(new ReversedList<>(this.list), Collections.reverseOrder(this.comparator));
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<E> subSet(
            final E fromElement,
            final boolean fromInclusive,
            final E toElement,
            final boolean toInclusive) {
        if (compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException("fromElement can't be greater than toElement");
        }
        final int from = convertIndex(fromElement, (fromInclusive ? 0 : 1), 0);
        final int to = Math.max(convertIndex(toElement, (toInclusive ? 1 : 0), 0), from);
        return new ArraySet<>(this, from, to);
    }

    @Override
    public SortedSet<E> subSet(final E fromElement, final E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public NavigableSet<E> headSet(final E toElement, final boolean inclusive) {
        if (this.isEmpty()) return this;
        if (compare(this.first(), toElement) > 0) {
            return subSet(this.first(), true, this.first(), false);
        }
        return subSet(this.first(), true, toElement, inclusive);
    }

    @Override
    public SortedSet<E> headSet(final E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public NavigableSet<E> tailSet(final E fromElement, final boolean inclusive) {
        if (this.isEmpty()) return this;
        if (compare(fromElement, this.last()) > 0) {
            return subSet(this.last(), false, this.last(), true);
        }
        return subSet(fromElement, inclusive, this.last(), true);
    }

    @Override
    public SortedSet<E> tailSet(final E fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public Comparator<E> comparator() {
        return this.comparator;
    }

    @Override
    public E first() {
        if (!this.list.isEmpty()) return this.list.get(0);
        throw new NoSuchElementException("can't get first element from an empty set");
    }

    @Override
    public E last() {
        if (!this.list.isEmpty()) return this.list.get(this.size() - 1);
        throw new NoSuchElementException("can't get last element from an empty set");
    }

    private int binarySearch(final E e) {
        return Collections.binarySearch(this.list, e, this.comparator);
    }

    @SuppressWarnings("unchecked")
    private int compare(final E first, final E second) {
        return this.comparator == null ? ((Comparable<E>) first).compareTo(second)
                : this.comparator.compare(first, second);
    }

    private E getElement(final int index) {
        return (0 <= index && index < size()) ? this.list.get(index) : null;
    }

    private int convertIndex(final E e, final int positiveShift, final int negativeShift) {
        final int index = binarySearch(e);
        return (index >= 0) ? (index + positiveShift) : (-index - 1 - negativeShift);
    }

    private static class ReversedList<E> extends AbstractList<E> {
        private final boolean isReversed;
        private final List<E> list;

        ReversedList(final List<E> list) {
            if (list instanceof ReversedList) {
                this.list = ((ReversedList<E>) list).list;
                this.isReversed = !((ReversedList<E>) list).isReversed;
            } else {
                this.list = list;
                this.isReversed = true;
            }
        }

        @Override
        public E get(final int index) {
            return this.list.get(this.isReversed ? (this.size() - 1 - index) : index);
        }

        @Override
        public int size() {
            return this.list.size();
        }
    }
}
