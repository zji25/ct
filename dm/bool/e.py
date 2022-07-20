from itertools import product

n = int(input())
f = []
for i in range(2 ** n):
    f.append(int(input()[-1]))

a = [0] * (2 ** n)
a[0] = f[0]
table = list(product([0, 1], repeat=n))

for i in range(1, 2 ** n):
    poly = f[0]
    for j in range(1, i):
        flag = True
        for k in range(n):
            if table[i][k] < table[j][k]:
                flag = False
                break
        if flag:
            poly = poly ^ a[j]
    if (f[i] == 0 and poly == 0) or (f[i] == 1 and poly == 1):
        a[i] = 0
    else:
        a[i] = 1

for i in range(2 ** n):
    print(''.join([str(j) for j in table[i]]) + ' ' + str(a[i]))