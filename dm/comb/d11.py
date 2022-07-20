def mm():
    n = int(input())
    a = []
    print()

    def f(q):
        for i in range(q + 1, n + 1):
            a.append(i)
            print(' '.join([str(_) for _ in a]))
            f(i)
            a.pop()

    f(0)


if __name__ == '__main__':
    mm()