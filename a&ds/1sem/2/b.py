def bbb():
    with open('input.txt', 'r') as inp:
        n, k = map(int, inp.readline().split())
        a = [0] + list(map(int, inp.readline().split())) + [0]
    dp = [0] * n
    path = [-1] * n
    for i in range(1, n):
        mm = -float('inf')
        for j in range(max(0, i - k), i):
            if dp[j] + a[i] > mm:
                mm = dp[j] + a[i]
                path[i] = j
        dp[i] = mm
    final_path = [n]
    pos = n - 1
    while pos != 0:
        final_path.append(path[pos] + 1)
        pos = path[pos]
    final_path.reverse()
    with open('output.txt', 'w') as out:
        out.write(str(dp[n - 1]) + '\n')
        out.write(str(len(final_path) - 1) + '\n')
        out.write(" ".join([str(j) for j in final_path]))


if __name__ == '__main__':
    bbb()
