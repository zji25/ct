n = int(input())
array = [[int(j) for j in bin(int(i))[2:]] for i in input().split()]
for i in range(n):
    array[i] = [0] * (33 - len(array[i])) + array[i]
b = int(input())
if b < 0 or b >= 2 ** 32:
    print('Impossible')
else:
    b = [int(j) for j in bin(b)[2:]]
    b = [0] * (33 - len(b)) + b
    f1 = []
    for i in range(33):
        if b[i] == 1:
            f1.append(i)
    if len(f1) == 0:
        print('~1&1')
    else:
        f = []
        for i in f1:
            string = []
            for j in range(n):
                string.append(array[j][i])
            f.append(string)
        output = []
        for i in range(len(f1)):
            out = ''
            for j in range(len(f[i])):
                if f[i][j] == 1:
                    out += str(j + 1)
                else:
                    out += '~' + str(j + 1)
                if j < len(f[i]) - 1:
                    out += '&'
            output.append(out)
        result = []
        for i in range(33):
            res_d = 0  # тут будет дизъюнкция потому что
            for j in output:
                res_k = 1  # а тут конъюнкция
                idk = j.split('&')
                for k in range(len(idk)):
                    if idk[k][0] != '~':
                        res_k *= array[int(idk[k]) - 1][i]
                    else:
                        res_k *= abs(array[int(idk[k][1:]) - 1][i] - 1)
                res_d = res_d or res_k
            result.append(int(res_d))
        if result == b:
            print('(' + ')|('.join(output) + ')')
        else:
            print('Impossible')
