n = int(input())
lst = []
for i in range(n):
    lst.append(input())
lst.sort()
print(lst[0])
dp = {lst[0]: 0}
for i in range(1, n):
    s = lst[i]
    ind = len(s)
    for j in range(len(s) - 1, 0, -1):
        if s[j] == '/':
            ind = j
            break
    left = s[:ind]
    right = s[ind + 1:]
    sp = dp[left] + 1
    print(' ' * sp * 2 + right)
    dp[s] = sp
