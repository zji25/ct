def f():
    n = int(input())
    s = '0' * n
    for i in range(2 ** n):
        print(s)
        count = n - 1
        for q in range(n - 1, -1, -1):
            if s[q] == '0':
                break
            count -= 1
        if count != -1:
            s = s[:count] + '1' + '0' * (n - count - 1)


if __name__ == '__main__':
    f()