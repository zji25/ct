def f():
    with open('problem2.in', 'r') as inp:
        w = inp.readline()[:-1]
        n, m, k = map(int, inp.readline().split())
        mp = {}
        acc = set(map(int, inp.readline().split()))
        for i in range(m):
            a, b, c = inp.readline().split()
            ac, b = (int(a) - 1, c), int(b) - 1
            if ac in mp:
                mp[ac].append(b)
            else:
                mp[ac] = [b]
        okk = [[0 for _ in range(n)] for _ in range(len(w) + 1)]
        okk[0][0] = 1
        for i in range(len(w)):
            for j in range(n):
                if okk[i][j] and (j, w[i]) in mp:
                    for v in mp[(j, w[i])]:
                        okk[i + 1][v] = 1
    with open('problem2.out', 'w') as out:
        for i in range(n):
            if okk[len(w)][i] == 1 and i + 1 in acc:
                out.write('Accepts\n')
                return
        out.write('Rejects\n')


if __name__ == '__main__':
    f()
