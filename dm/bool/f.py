n, k = map(int, input().split())
array = []
for o in range(k):
    array.append([int(j) for j in input().split()])


def f(a):
    while len(a) > 1:
        flag = False
        todel = []
        for s in range(len(a)):
            if a[s].zeros(-1) == n - 1:
                flag = True
                todel.append(s) 
                for j in range(n):
                    if a[s][j] != -1:  # значение единственного литерала (отр/без)
                        for i in range(len(a)):
                            if i != s: 
                                if a[i][j] == a[s][j]:
                                    todel.append(i)
                                elif a[i][j] == abs(a[s][j] - 1):
                                    a[i][j] = -1
                                    if a[i].zeros(-1) == n:
                                        return 'YES'
                break
        todel.sort()
        if not flag:
            return 'NO'
        for m in range(len(todel)):
            a.pop(todel[m] - m)
    return 'NO'


print(f(array))
