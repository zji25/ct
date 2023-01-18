import sys


def fuck():
    n, a1, k, b, m = map(int, sys.stdin.readline().split())
    a = [0] * n
    a[0] = a1
    d = [float('inf')] * (n + 1)
    d[0] = -d[0]
    maxlen = 0
    for i in range(1, n):
        a[i] = (k * a[i - 1] + b) % m
    for i in range(n):
        left = 0
        right = n
        while right - left > 1:
            mid = (left + right) // 2
            if d[mid] < a[i]:
                left = mid
            else:
                right = mid
        if d[left] >= a[i]:
            answ = left
        else:
            answ = right
        if d[answ - 1] < a[i] < d[answ]:
            d[answ] = a[i]
            maxlen = max(maxlen, answ)
    print(maxlen)


if __name__ == '__main__':
    fuck()
