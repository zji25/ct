n = int(input())
a = [int(i) for i in input().split()]


def mergesort(array):
    if len(array) <= 1:
        return array
    l = mergesort(array[:len(array) // 2])
    r = mergesort(array[len(array) // 2:])
    return merge(l, r)


def merge(l, r):
    array = []
    i, j = 0, 0
    while i < len(l) and j < len(r):
        if l[i] <= r[j]:
            array.append(l[i])
            i += 1
        else:
            array.append(r[j])
            j += 1
    while i < len(l):
        array.append(l[i])
        i += 1
    while j < len(r):
        array.append(r[j])
        j += 1
    return array


print(' '.join([str(i) for i in mergesort(a)]))
