def f():
    with open('epsilon.in', 'r') as inp:
        n, s = inp.readline().split()
        n = int(n)
        acc = set()
        rls = []
        for i in range(n):
            za = inp.readline().split()
            a = za[0]
            c = ''
            if len(za) == 3:
                c = za[2]
            if len(c) == 0:
                acc.add(a)
            else:
                rls.append((a, c))
        while 1:
            ui = len(acc)
            for rl in rls:
                for j in range(len(rl[1])):
                    if not rl[1][j] in acc or rl[1][j].islower():
                        break
                else:
                    acc.add(rl[0])
            if len(acc) == ui:
                break
        ske = list(acc)
        ske.sort()
    with open('epsilon.out', 'w') as out:
        out.write(' '.join(ske))


if __name__ == '__main__':
    f()
