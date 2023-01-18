n = int(input())
a = [int(i) for i in input().split()]
for i in range(1, n, 2):
    if (i + 1) < n:
        if a[i] < a[(i - 1)//2] or a[i + 1] < a[(i - 1)//2]:
            print('NO')
            break
    elif a[i] < a[(i - 1)//2]:
        print('NO')
        break
else:
    print('YES')
