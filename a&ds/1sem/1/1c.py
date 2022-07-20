n = int(input())
a = [int(i) for i in input().split()]


def merge(array, l, r):
    counter = 0
    i, j = 0, 0

    while i < len(l) and j < len(r):
        if l[i] <= r[j]:
            array[i + j] = l[i]
            i += 1
        else:
            array[i + j] = r[j]
            counter += len(l) - i
            j += 1
    while i < len(l):
        array[i + j] = l[i]
        i += 1
    while j < len(r):
        array[i + j] = r[j]
        j += 1
    return counter


def f(array):
    if len(array) <= 1:
        return 0
    p = array[:len(array) // 2]
    q = array[len(array) // 2:]
    return f(p) + f(q) + merge(array, p, q)


print(f(a))
