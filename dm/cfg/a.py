def f():
    with open('automaton.in', 'r') as inp:
        n, s = inp.readline().split()
        cnd = [[] for _ in range(26)]
        for i in range(int(n)):
            a, b, c = inp.readline().split()
            if len(c) == 0:
                continue
            if len(c) == 1:
                ac = (c[0])
            else:
                ac = (c[0], c[1])
            cnd[ord(a) - 65].append(ac)
        m = int(inp.readline())
        res = ''
        for r in range(m):
            w = inp.readline()[:-1]
            q = [(s, w, 0)]
            yes = 0
            noo = 1
            while len(q):
                ss, ww, index = q.pop()
                if index == len(ww):
                    noo = 1
                    break
                for ot in cnd[ord(ss) - 65]:
                    if ot[0] == ww[index] and len(ot) == 2:
                        q.append((ot[1], ww, index + 1))
                    elif ot[0] == ww[index] and index == len(ww) - 1:
                        noo = 0
                        yes = 1
                        break
                if yes:
                    res += 'yes\n'
                    noo = 0
                    break
            if noo:
                res += 'no\n'

    with open('automaton.out', 'w') as out:
        out.write(res)


if __name__ == '__main__':
    f()
