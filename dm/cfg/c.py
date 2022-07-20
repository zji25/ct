def f():
    with open('useless.in', 'r') as inp:
        n, s = inp.readline().split()
        n = int(n)
        acc = set()
        jvu = set()
        rls = []
        for i in range(n):
            zzz = inp.readline().split()
            a = zzz[0]
            c = ''
            jvu.add(a)
            if len(zzz) == 3:
                c = zzz[2]
            if len(c) == 0 or c.islower():
                acc.add(a)
            rls.append((a, c))
            for cc in c:
                if cc.isupper():
                    jvu.add(cc)
        while 1:
            ui = len(acc)
            for rl in rls:
                for j in range(len(rl[1])):
                    if rl[1][j].isupper() and rl[1][j] not in acc:
                        break
                else:
                    acc.add(rl[0])
            if len(acc) == ui:
                break
        if s not in acc:
            jvu.add(s)
            ske = sorted(list(jvu))
            with open('useless.out', 'w') as out:
                out.write(' '.join(ske))
            return
        rls2 = []
        for rl in rls:
            if rl[0] not in acc:
                continue
            for j in rl[1]:
                if j not in acc and j.isupper():
                    break
            else:
                rls2.append(rl)
        occ = [0 for _ in range(26)]
        occ[ord(s) - 65] = 1
        while 1:
            flg = 0
            for rl in rls2:
                if occ[ord(rl[0]) - 65]:
                    for j in rl[1]:
                        if j.isupper() and not occ[ord(j) - 65]:
                            occ[ord(j) - 65] = 1
                            flg = 1
            if not flg:
                break
        for i in range(26):
            if occ[i]:
                jvu.remove(chr(i + 65))
        ske = sorted(list(jvu))

    with open('useless.out', 'w') as out:
        out.write(' '.join(ske))


if __name__ == '__main__':
    f()
