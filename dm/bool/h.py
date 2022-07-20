def f(m, s):
    if m == 1:
        return '((A0|B0)|(A0|B0))'
    else:
        n = str(m - 1)
        return '((' + f(m - 1, s) + '|((A' + n + '|A' + n + ')|(B' + n + '|B' + n + ')))|(A' + n + '|B' + n + '))'


nn = int(input())
print(f(nn, ''))

