def f():
    with open('cf.in', 'r') as inp:
        r, start = inp.readline().split()
        r, start = int(r), ord(start) - 65
        rules = []
        eps = set()
        net_counter = 26
        for i in range(r):
            b = inp.readline().split()
            a, c = ord(b[0]) - 65, ''
            if len(b) == 3:
                c = b[2]
            if len(c) == 0:
                rules.append((a, ''))
                eps.add(a)
            elif c.isupper():
                rules.append((a, [ord(s) - 65 for s in c]))
            elif c.islower() and len(c) == 1:
                rules.append((a, c))
            else:
                cc = []
                for s in c:
                    if s.islower():
                        for rule in rules:
                            if len(rule[1]) == 1 and rule[1] == s:
                                cc.append(rule[0])
                                break
                        else:
                            rules.append([net_counter, s])
                            cc.append(net_counter)
                            net_counter += 1
                    else:
                        cc.append(ord(s) - 65)
                rules.append((a, cc))
        w = inp.readline()
        if w[-1] == '\n':
            w = w[:-1]
    while 1:
        brk = 1
        for i in range(len(rules)):
            if len(rules[i][1]) >= 3:
                rules.append((net_counter, rules[i][1][1:]))
                rules[i] = (rules[i][0], [rules[i][1][0], net_counter])
                net_counter += 1
                brk = 0
                break
        if brk:
            break
    while 1:
        ui = len(eps)
        for rule in rules:
            if type(rule[1]) != str:
                for j in range(len(rule[1])):
                    if rule[1][j] in eps:
                        eps.add(rule[0])
        if len(eps) == ui:
            break
    while 1:
        ui = 1
        for i in range(len(rules)):
            if len(rules[i][1]) == 2:
                for j in range(2):
                    if rules[i][1][j] in eps:
                        zz = list(rules[i][1])
                        zz.pop(j)
                        zzq = (rules[i][0], zz)
                        if zzq not in rules:
                            rules.append(zzq)
                            ui = 0
                            break
        if ui:
            break
    nrs = []
    dl = []
    tw = [0 for _ in range(net_counter)]

    def ff(non):
        if tw[non]:
            return []
        aabb = []
        tw[non] = 1
        for ii in range(len(rules)):
            if ii not in dl:
                if rules[ii][0] == non:
                    if type(rules[ii][1]) == str or len(rules[ii][1]) == 2:
                        aabb.append(rules[ii][1])
                    else:
                        aabb.extend(ff(rules[ii][1][0]))
        tw[non] = 0
        return aabb

    for ij in range(len(rules)):
        if (ij not in dl) and type(rules[ij][1]) != str and len(rules[ij][1]) == 1:
            ab = ff(rules[ij][1][0])
            dl.append(ij)
            for j in ab:
                nrs.append([rules[ij][0], j])
    dl.sort()
    dl.reverse()
    for i in dl:
        rules.pop(i)
    for jho in nrs:
        if jho not in rules:
            rules.append(jho)
    wru = rules.copy()
    for www in wru:
        if len(www[1]) == 0:
            rules.remove(www)
    abc = [[] for _ in range(net_counter)]
    aa = [[] for _ in range(net_counter)]
    for rule in rules:
        if type(rule[1]) == str:
            if rule[1] not in aa[rule[0]]:
                aa[rule[0]].append(rule[1])
        else:
            abc[rule[0]].append(rule[1])
    pp = [[[0 for _ in range(len(w))] for _ in range(len(w))] for _ in range(net_counter)]
    for sq in range(len(w)):
        for iq in range(net_counter):
            for raa in aa[iq]:
                if raa == w[sq]:
                    pp[iq][sq][sq] = 1
    for ll in range(1, len(w)):
        for s in range(len(w) - ll):
            for i in range(net_counter):
                rr = 0
                for rbc in abc[i]:
                    rb, rc = rbc
                    for k in range(s, s + ll):
                        if pp[rb][s][k] * pp[rc][k + 1][s + ll] > 0:
                            rr = 1
                pp[i][s][s + ll] = int(pp[i][s][s + ll] or rr)
    with open('cf.out', 'w') as out:
        if pp[start][0][len(w) - 1]:
            out.write('yes\n')
        else:
            out.write('no\n')


if __name__ == '__main__':
    f()
