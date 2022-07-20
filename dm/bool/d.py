n = int(input())
f = []
for i in range(2 ** n):
    s = input().split()
    if s[1] == '1':
        f.append(s[0])

if len(f) != 0:
    # сднф
    output = [[1, i] for i in range(1, n + 1)]
    last_index = 0
    for i in range(len(f)):  # по кажд строчке таблицы = 1
        out = []
        for j in range(n):
            if f[i][j] == '1':
                out.append(j + 1)  # без инверсии
            else:
                out.append(n + j + 1)  # с инверсией
        if last_index == 0:
            output.append([2, out[0], out[1]])
            for k in range(2, len(out)):
                output.append([2, out[k], len(output) + n])
            last_index = len(output) + n
        else:
            output.append([2, out[0], out[1]])
            for k in range(2, len(out)):
                output.append([2, out[k], len(output) + n])
            output.append([3, last_index, len(output) + n])
            last_index = len(output) + n
else:
    output = [[1, 1], [2, 1, n + 1]]

print(len(output) + n)
for i in output:
    print(' '.join(str(j) for j in i))
