import sys
last = -1


def ddd():
    n, q = map(int, sys.stdin.readline().split())
    a = [i + 1 for i in range(n)]
    a = mergesort(a)
    b = [0] * n
    for i in range(n):
        b[a[i] - 1] = i + 1
    res = '!'
    for i in range(n):
        res += ' ' + str(b[i])
    print(res)
    sys.stdout.flush()


def mergesort(a):
    if len(a) <= 1:
        return a
    l = mergesort(a[:len(a) // 2])
    r = mergesort(a[len(a) // 2:])
    return merge(l, r)


def merge(aa, bb):
    array = []
    i, j = 0, 0
    while i < len(aa) and j < len(bb):
        if f(bb[j], aa[i]):
            array.append(bb[j])
            j += 1
        else:
            array.append(aa[i])
            i += 1
    while i < len(aa):
        array.append(aa[i])
        i += 1
    while j < len(bb):
        array.append(bb[j])
        j += 1
    return array


def f(y, x):
    global last
    if y == last:
        print('+ ' + str(x))
        sys.stdout.flush()
        last = x
        s = sys.stdin.readline()
        return s == 'NO\n'
    if x == last:
        print('+ ' + str(y))
        sys.stdout.flush()
        last = y
        s = sys.stdin.readline()
        return s == 'YES\n'
    print('+ ' + str(y))
    sys.stdout.flush()
    sys.stdin.readline()
    print('+ ' + str(x))
    sys.stdout.flush()
    s = sys.stdin.readline()
    last = x
    return s == 'NO\n'


if __name__ == '__main__':
    ddd()
