import sys


def sesh(a, b):
    if a == 0:
        return b
    while b:
        a, b = b, a % b
    return a


def fuck():
    n, d = map(int, sys.stdin.readline().split())
    a = list(map(int, sys.stdin.readline().split()))
    q = 0
    res = []
    for i in range(n):
        if not a[i] % d:
            res.append(a[i])
            q = sesh(q, a[i])
    if len(res) == 0 or d != q:
        sys.stdout.write('-1')
        return
    sys.stdout.write(str(len(res)) + '\n')
    sys.stdout.write(' '.join([str(i) for i in res]))


if __name__ == '__main__':
    fuck()
