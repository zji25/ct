def f():
    with open('isomorphism.in', 'r') as inp:
        n1, m1, k1 = map(int, inp.readline().split())
        acc1 = set(map(int, inp.readline().split()))
        okk1 = [[-1 for _ in range(26)] for _ in range(n1)]
        for _ in range(m1):
            a, b, c = inp.readline().split()
            okk1[int(a) - 1][ord(c) - 97] = int(b) - 1
        n2, m2, k2 = map(int, inp.readline().split())
        acc2 = set(map(int, inp.readline().split()))
        okk2 = [[-1 for _ in range(26)] for _ in range(n2)]
        for _ in range(m2):
            a, b, c = inp.readline().split()
            okk2[int(a) - 1][ord(c) - 97] = int(b) - 1
        visited = [0 for _ in range(max(n1, n2))]

    def dfs(u, v):
        if visited[u]:
            return 1
        visited[u] = 1
        if (u + 1 in acc1) != (v + 1 in acc2):
            return 0
        if okk1[u].count(-1) != okk2[v].count(-1):
            return 0
        if okk1[u].count(-1) == 26:
            return 1
        for j in range(26):
            if (okk1[u][j] == -1) != (okk2[v][j] == -1):
                return 0
            if okk1[u][j] + okk2[v][j] != -2:
                if not dfs(okk1[u][j], okk2[v][j]):
                    return 0
        return 1

    with open('isomorphism.out', 'w') as out:
        if dfs(0, 0):
            out.write('YES\n')
        else:
            out.write('NO\n')


if __name__ == '__main__':
    f()
