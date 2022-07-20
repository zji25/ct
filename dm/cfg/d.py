def f():
    with open('nfc.in', 'r') as inp:
        r, sss = inp.readline().split()
        r = int(r)
        abc = [[] for _ in range(26)]
        aa = [[] for _ in range(26)]
        for i in range(r):
            a, b, c = inp.readline().split()
            if len(c) == 2:
                abc[ord(a) - 65].append((ord(c[0]) - 65, ord(c[1]) - 65))
            else:
                aa[ord(a) - 65].append(ord(c) - 97)
        w = inp.readline()
        if w[-1] == '\n':
            w = w[:-1]
    pp = [[[0 for _ in range(len(w))] for _ in range(len(w))] for _ in range(26)]
    for s in range(len(w)):
        for i in range(26):
            for raa in aa[i]:
                if raa == ord(w[s]) - 97:
                    pp[i][s][s] = 1
    for l in range(1, len(w)):
        for s in range(len(w) - l):
            for i in range(26):
                rr = 0
                for rbc in abc[i]:
                    rb, rc = rbc
                    for k in range(s, s + l):
                        rr = (rr + (pp[rb][s][k] * pp[rc][k + 1][s + l]) % 1000000007) % 1000000007
                pp[i][s][s + l] = rr
    res = pp[ord(sss) - 65][0][len(w) - 1]
    with open('nfc.out', 'w') as out:
        out.write(str(res))


if __name__ == '__main__':
    f()
