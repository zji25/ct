def mm():
    n, k = map(int, input().split())
    a = [i for i in range(1, k + 1)]
    print(" ".join([str(_) for _ in a]))

    def fuc(a):
        for i in range(k - 1, -1, -1):
            if a[i] < n - k + i + 1:
                a[i] += 1
                for j in range(i + 1, k):
                    a[j] = a[j - 1] + 1
                return a
        return "nope"

    while fuc(a) != "nope":
        print(" ".join([str(_) for _ in a]))


if __name__ == '__main__':
    mm()