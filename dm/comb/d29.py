def mm():
    s = input().split("=")
    n = s[0]
    a = [int(_) for _ in s[1].split("+")]
    lg = len(a)
    if lg == 1:
        print("No solution")
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
    print(n + "=" + "+".join([str(_) for _ in a]))


if __name__ == '__main__':
    mm()