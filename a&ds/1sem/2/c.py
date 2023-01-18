dp = [0] * 101
dp[1] = 2
dp[2] = 3


def f(x):
    global dp
    if dp[x] != 0 or x == 0:
        return dp[x]
    dp[x] = f(x - 1) + f(x - 2)
    return dp[x]


def ccc():
    with open('input.txt', 'r') as inp:
        res = f(int(inp.readline()))
    with open('output.txt', 'w') as out:
        out.write(str(res))


if __name__ == '__main__':
    ccc()
