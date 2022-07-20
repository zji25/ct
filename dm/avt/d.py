def mul(a, b, n):
    res = [[0 for _ in range(n)] for _ in range(n)]
    for i in range(n):
        for j in range(n):
            for k in range(n):
                res[i][j] += a[i][k] * b[k][j]
    return res


def pp(a, p, n):
    if p == 0:
        mx = [[0 for _ in range(n)] for _ in range(n)]
        for i in range(n):
            mx[i][i] = 1
        return mx
    x = pp(a, p // 2, n)
    x = mul(x, x, n)
    if p % 2 == 1:
        x = mul(x, a, n)
    return x


def f():
    with open('problem4.in', 'r') as inp:
        n, m, k, l = map(int, inp.readline().split())
        okk = [[0 for _ in range(n)] for _ in range(n)]
        acc = set(map(int, inp.readline().split()))
        for i in range(m):
            a, b, _ = inp.readline().split()
            okk[int(a) - 1][int(b) - 1] += 1
        ok = pp(okk, l, n)
        res = 0
        for i in range(n):
            if i + 1 in acc:
                res = (res + ok[0][i]) % 1000000007
    with open('problem4.out', 'w') as out:
        out.write(str(res))


if __name__ == '__main__':
    f()
