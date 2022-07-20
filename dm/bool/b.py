from itertools import product

n = int(input())
flg_s0, flg_s1, flg_mon, flg_sm, flg_lin = 0, 0, 0, 0, 0


def is_mon(args, s): 
    table = list(product('01', repeat=args))
    for j in range(len(s)):  
        for k in range(j + 1, len(s)): 
            for e in range(args): 
                if table[j][e] > table[k][e]:
                    break
            else: 
                if s[j] > s[k]:
                    return 1
    return 0


def is_lin(args, s):
    table = list(product([0, 1], repeat=args))
    a = [0] * (2 ** args)
    a[0] = int(s[0])
    for i in range(1, 2 ** args):
        poly = int(s[0])
        for j in range(1, i):
            local_flag = True
            for k in range(args):
                if table[i][k] < table[j][k]:
                    local_flag = False
                    break
            if local_flag:
                poly = poly ^ a[j]
        if (s[i] == '0' and poly == 0) or (s[i] == '1' and poly == 1):
            a[i] = 0
        else:
            a[i] = 1
        if table[i].count(1) > 1 and a[i] == 1:
            return 1
    return 0


def is_smdv(args, s):
    cur_flag_sm = 0
    rvrs = s[::-1]
    for j in range(len(s)):
        if s[j] == rvrs[j]:
            cur_flag_sm = 1
            break
    return cur_flag_sm


for i in range(n):
    amount_of_args, sfunc = input().split()
    amount_of_args = int(amount_of_args)
    if flg_s0 == 0:
        if len(sfunc) > 0 and sfunc[0] == '1':
            flg_s0 = 1
    if flg_s1 == 0:
        if len(sfunc) > 0 and sfunc[-1] == '0':
            flg_s1 = 1
    if flg_mon == 0:
        flg_mon = is_mon(amount_of_args, sfunc)
    if flg_sm == 0:
        flg_sm = is_smdv(amount_of_args, sfunc)
    if flg_lin == 0:
        flg_lin = is_lin(amount_of_args, sfunc)
    if flg_s0 + flg_s1 + flg_mon + flg_sm + flg_lin > 4:
        print('YES')
        break
else:
    print('NO')
