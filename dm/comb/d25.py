def mm():
    n, k = map(int, input().split())
    a = list(map(int, input().split()))
    for i in range(k - 1, -1, -1):
        if a[i] <= n - k + i:
            a[i] += 1
            for j in range(i + 1, k):
                a[j] = a[j - 1] + 1
            print(" ".join([str(_) for _ in a]))
            return
    print(-1)


if __name__ == '__main__':
    mm()
