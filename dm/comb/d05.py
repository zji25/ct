from math import factorial


def mm():
    n, k = map(int, input().split())
    res = [[0] * n for _ in range(k ** n)]
    for j in range(n):
        q = 0
        p = -1
        for i in range(k ** n):
            if i % (k ** j) == 0:
                if (i // (k ** j)) % k == 0:
                    p = -p
                else:
                    q += p
            res[i][n - j - 1] = q
    for i in range(k ** n):
        print("".join([str(_) for _ in res[i]]))


if __name__ == '__main__':
    print((1/21) * (factorial(40)/(factorial(20)*factorial(20))) * (2 ** 20))
    mm()
