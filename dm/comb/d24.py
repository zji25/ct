def mm():
    n = int(input())
    p = list(map(int, input().split()))
    a = p.copy()
    ind = -1
    mx = -1
    for j in range(n - 2, -1, -1):
        if p[j] > p[j + 1]:
            mx = j
            break
    if mx == -1:
        print(' '.join(['0'] * n))
    else:
        for j in range(mx + 1, n):
            if p[mx] > p[j]:
                ind = j
        p[mx], p[ind] = p[ind], p[mx]
        p = p[:mx + 1] + [p[i] for i in range(n - 1, mx, -1)]
        print(' '.join([str(_) for _ in p]))

    mx = -1
    for j in range(n - 2, -1, -1):
        if a[j] < a[j + 1]:
            mx = j
            break
    if mx == -1:
        print(' '.join(['0'] * n))
        return
    ind = -1
    for j in range(mx + 1, n):
        if a[j] > a[mx]:
            ind = j
    a[ind], a[mx] = a[mx], a[ind]
    a = a[:mx + 1] + [a[i] for i in range(n - 1, mx, -1)]
    print(' '.join([str(_) for _ in a]))


if __name__ == '__main__':
    mm()
