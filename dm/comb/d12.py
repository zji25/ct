def mm():
    n, k = map(int, input().split())
    a = [[] for _ in range(k)]

    def f(t, am):
        if t == n:
            if am == k:
                print('\n'.join(' '.join([str(_) for _ in qq]) for qq in a) + '\n')
            return
        if am < k:
            a[am].append(t + 1)
            f(t + 1, am + 1)
            a[am].pop()
        for i in range(am):
            a[i].append(t + 1)
            f(t + 1, am)
            a[i].pop()

    f(0, 0)


if __name__ == '__main__':
    mm()