from math import factorial


def mm():
    n = int(input())
    a = [i for i in range(1, n + 1)]
    print(' '.join([str(_) for _ in a]))
    for i in range(factorial(n)):
        ind = -1
        for j in range(n - 2, -1, -1):
            if a[j] < a[j + 1]:
                ind = j
                break
        if ind == -1:
            break
        mx = -1
        for j in range(ind + 1, n):
            if a[j] > a[ind]:
                mx = j
        a[mx], a[ind] = a[ind], a[mx]
        a = a[:ind + 1] + [a[i] for i in range(n - 1, ind, -1)]
        print(' '.join([str(_) for _ in a]))


if __name__ == '__main__':
    mm()