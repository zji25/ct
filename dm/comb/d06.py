def bb(x):
    res = ''
    while x > 0:
        res += str(x % 2)
        x //= 2
    return res[::-1]


def mm():
    n = int(input())
    res = []
    for i in range(2 ** n):
        s = bb(i)
        for j in range(len(s) - 1):
            if s[j] == s[j + 1] == '1':
                break
        else:
            while len(s) < n:
                s = '0' + s
            res.append(s)
    print(str(len(res)))
    print('\n'.join(res))


if __name__ == '__main__':
    mm()