import sys


def fuck():
    n = int(sys.stdin.readline())
    dd = [q for q in range(n + 1)]
    a = [int(q) - 1 for q in sys.stdin.readline().split()] + [0]
    res = []

    def f(x):
        x = gg(x)
        dd[x] = gg((x + 1) % n)

    def gg(x):
        if dd[x] == x:
            return x
        dd[x] = gg(dd[x])
        return dd[x]

    for i in range(n):
        p = gg(a[i])
        f(p)
        res.append(p)
    sys.stdout.write(' '.join([str(_ + 1) for _ in res]))


if __name__ == '__main__':
    fuck()
