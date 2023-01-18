n = int(input())
a = [int(i) for i in input().split()]
b = []
c = []
counter = 1
flag = True
for elem in a:
    if len(b) > 0 and elem > b[-1]:
        flag = False
        break
    c.append(1)
    b.append(elem)
    while len(b) > 0 and b[-1] == counter:
        c.append(2)
        b.pop()
        counter += 1
if flag:
    elem = c.pop(0)
    counter = 1
    while len(c) > 0:
        if c[0] == elem:
            counter += 1
        else:
            print(str(elem) + ' ' + str(counter))
            elem = c[0]
            counter = 1
        c.pop(0)
    print(str(elem) + ' ' + str(counter))
else:
    print(0)
