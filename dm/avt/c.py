def f():
    with open('problem3.in', 'r') as inp:
        n, m, k = map(int, inp.readline().split())
        acc = set(map(int, inp.readline().split()))
        g = [[] for _ in range(n)]
        back = [[] for _ in range(n)]
        for _ in range(m):
            a, b, _ = inp.readline().split()
            a, b = int(a) - 1, int(b) - 1
            g[a].append(b)
            back[b].append(a)
    visited = [0 for _ in range(n)]

    def dfs(t):
        if visited[t]:
            return
        visited[t] = 1
        for u in back[t]:
            dfs(u)

    for a in acc:
        dfs(a - 1)
    clrd = [-1 for _ in range(n)]
    ans = []

    def srt(t):
        if clrd[t] == 1:
            return 0
        clrd[t] = 0
        for u in g[t]:
            if visited[u]:
                if clrd[u] == 0:
                    return 1
                if clrd[u] == -1:
                    if srt(u):
                        return 1
        clrd[t] = 1
        ans.append(t)

    if srt(0):
        return -1
    paths = [0 for _ in range(n)]
    paths[0] = 1
    res = 0
    for i in reversed(ans):
        for j in g[i]:
            if visited[j]:
                paths[j] = (paths[j] + paths[i]) % 1000000007
    for i in acc:
        res = (res + paths[i - 1]) % 1000000007
    return res


if __name__ == '__main__':
    with open('problem3.out', 'w') as out:
        z = str(f())
        out.write(z)
        print(z)
