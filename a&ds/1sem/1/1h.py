string = input()
flags = ''
d = {'(': ')', '[': ']', '{': '}'}
for i in range(len(string)):
    if string[i] == '(' or string[i] == '[' or string[i] == '{':
        flags += string[i]
    else:
        if len(flags) > 0:
            if string[i] == d[flags[-1]]:
                flags = flags[:-1]
        else:
            print('NO')
            break
else:
    if len(flags) != 0:
        print('NO')
    else:
        print('YES')
