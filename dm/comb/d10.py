def mm():
    n = int(input())
    a = [1] * n
    lg = n
    while lg > 0:
        print("+".join([str(_) for _ in a]))
        if lg == 1:
            return
        a[lg - 2] += 1
        a[lg - 1] -= 1
        if a[lg - 2] > a[lg - 1]:
            a[lg - 2] += a[lg - 1]
            a.pop()
            lg -= 1
        else:
            while a[lg - 1] >= a[lg - 2] * 2:
                a.append(a[lg - 1] - a[lg - 2])
                lg += 1
                a[lg - 2] = a[lg - 3]


if __name__ == '__main__':
    mm()
