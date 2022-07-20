import sys


def fuck():
    n = int(sys.stdin.readline())
    mx = 2 * 1000000 + 1
    a = [-1] * mx
    a[1] = 1
    for i in range(2, mx):
        if a[i] == -1:
            for j in range(i, mx, i):
                a[j] = i
    for _ in range(n):
        x = int(sys.stdin.readline())
        res = []
        while x != 1:
            d = a[x]
            x //= d
            res.append(d)
        print(*reversed(res))


if __name__ == '__main__':
    fuck()
