def f():
    with open('equivalence.in', 'r') as inp:
        n1, m1, k1 = map(int, inp.readline().split())
        acc1 = set(map(int, inp.readline().split()))
        okk1 = [[0 for _ in range(26)] for _ in range(n1 + 1)]
        for _ in range(m1):
            a, b, c = inp.readline().split()
            okk1[int(a)][ord(c) - 97] = int(b)
        n2, m2, k2 = map(int, inp.readline().split())
        acc2 = set(map(int, inp.readline().split()))
        okk2 = [[0 for _ in range(26)] for _ in range(n2 + 1)]
        for _ in range(m2):
            a, b, c = inp.readline().split()
            okk2[int(a)][ord(c) - 97] = int(b)
        visited = [[0 for _ in range(n2 + 1)] for _ in range(n1 + 1)]

    def bfs():
        q = [(1, 1)]
        while len(q) > 0:
            u, v = q.pop()
            if (u in acc1) != (v in acc2):
                return 0
            visited[u][v] = 1
            for cc in range(26):
                if not visited[okk1[u][cc]][okk2[v][cc]]:
                    q.append((okk1[u][cc], okk2[v][cc]))
        return 1

    with open('equivalence.out', 'w') as out:
        if bfs():
            out.write('YES\n')
        else:
            out.write('NO\n')


if __name__ == '__main__':
    f()
