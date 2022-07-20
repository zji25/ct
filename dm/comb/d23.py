def mm():
    s = input()
    lg = len(s)
    count = lg - 1
    for i in range(lg - 1, -1, -1):
        if s[i] != '0':
            break
        count -= 1
    print('-' if count == -1 else s[:count] + '0' + '1' * (lg - count - 1))
    count = lg - 1
    for i in range(lg - 1, -1, -1):
        if s[i] != '1':
            break
        count -= 1
    print('-' if count == -1 else s[:count] + '1' + '0' * (lg - count - 1))


if __name__ == '__main__':
    mm()
