n = int(input())

r0 = []
r1 = []

for o in 0, 1:
    for _ in range(n):
        string = [int(o) for o in input().split()]
        if o == 0:
            r0.append(string)
        else:
            r1.append(string)


def is_ref(rel):
    for i in range(n):
        if rel[i][i] == 0:
            return 0
    return 1


def is_anti_ref(rel):
    for i in range(n):
        if rel[i][i] == 1:
            return 0
    return 1


def is_sym(rel):
    for k in range(n):
        for m in range(n):
            if rel[k][m] != rel[m][k]:
                return 0
    return 1


def is_anti_sym(rel):
    for k in range(n):
        for m in range(n):
            if k != m and rel[k][m] == rel[m][k] and rel[m][k] == 1:
                return 0
    return 1


def is_trans(rel):
    for m0 in range(n):
        for m1 in range(n):
            if rel[m0][m1] == 1:
                l0 = m1
                for l1 in range(n):
                    if rel[l0][l1] == 1 and rel[m0][l1] == 0:
                        return 0
    return 1


rel0 = [is_ref(r0), is_anti_ref(r0), is_sym(r0), is_anti_sym(r0), is_trans(r0)]
rel1 = [is_ref(r1), is_anti_ref(r1), is_sym(r1), is_anti_sym(r1), is_trans(r1)]
print(' '.join([str(_) for _ in rel0]))
print(' '.join([str(_) for _ in rel1]))

comp = [[0 for _ in range(n)] for _ in range(n)]

for i in range(n):
    for j in range(n):
        if r0[i][j] == 1:
            for m in range(n):
                if r1[j][m] == 1:
                    comp[i][m] = 1
for i in range(n):
    print(' '.join([str(_) for _ in comp[i]]))