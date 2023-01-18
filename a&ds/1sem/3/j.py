import sys
from functools import reduce


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


def fuck():
    n = (sys.stdin.readline())
    a = list(map(int, sys.stdin.readline().split()))
    g = abs(reduce(gcd, a))
    print(g)


if __name__ == '__main__':
    fuck()
