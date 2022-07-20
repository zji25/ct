from math import factorial


def mm():
    n, k = map(int, input().split())
    res = [0] * n
    used = [False] * n
    for i in range(n):
        pp = k // factorial(n - i - 1)
        k = k % factorial(n - i - 1)
        cur = 0
        for j in range(n):
            if not used[j]:
                cur += 1
                if cur == pp + 1:
                    res[i] = j + 1
                    used[j] = True
    print(" ".join([str(q) for q in res]))


if __name__ == '__main__':
    mm()
