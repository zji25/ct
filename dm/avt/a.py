def f():
    with open('problem1.in', 'r') as inp:
        w = inp.readline()[:-1]
        n, m, k = map(int, inp.readline().split())
        acc = set(map(int, inp.readline().split()))
        tab = [[100 for _ in range(26)] for _ in range(n)]
        for i in range(m):
            a, b, c = inp.readline().split()
            tab[int(a) - 1][ord(c) - 97] = int(b) - 1
    with open('problem1.out', 'w') as out:
        c = 0
        for i in range(len(w)):
            t = tab[c][ord(w[i]) - 97]
            if t == 100:
                out.write('Rejects\n')
                return
            c = t
        if c + 1 in acc:
            out.write('Accepts\n')
            return
        out.write('Rejects\n')


if __name__ == '__main__':
    f()
