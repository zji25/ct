dp = [-1] * 47
dp[0] = 0
dp[1] = 1


def f(x):
    global dp
    if dp[x] == -1:
        dp[x] = f(x - 1) + f(x - 2)
    return dp[x]


def aaa():
    with open('input.txt', 'r') as inp:
        res = f(int(inp.readline()))
    with open('output.txt', 'w') as out:
        out.write(str(res))


if __name__ == '__main__':
    aaa()
