from math import factorial


def mm():
    n = int(input())
    a = list(map(int, input().split()))
    used = [False] * n
    index = 0
    for i in range(n):
        for j in range(a[i] - 1):
            if not used[j]:
                index += factorial(n - i - 1)
        used[a[i] - 1] = True
    print(index)


if __name__ == '__main__':
    mm()
