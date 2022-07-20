def mm():
    n = int(input())
    cur = '0' * n
    s = set()
    s.add(cur)
    res = [cur]
    while True:
        p = cur[1:]
        if p + '1' not in s:
            cur = p + '1'
        elif p + '0' not in s:
            cur = p + '0'
        else:
            break
        res.append(cur)
        s.add(cur)
    print('\n'.join(res))


if __name__ == '__main__':
    mm()