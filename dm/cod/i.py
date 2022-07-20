n = int(input())
s = [int(i) for i in input()]
s.insert(0, 0)
lg = len(s)
if n == 1:
    r_bits_positions = []
    r_bits_amount = 0
    for o in range(lg):
        if 2 ** o < lg + o:
            r_bits_positions.append(2 ** o)
        else:
            r_bits_amount = o
            break
    s1 = []
    k = 0
    for o in range(lg + r_bits_amount):
        if o in r_bits_positions:
            s1.append(0)
        else:
            s1.append(s[k])
            k += 1
    for pos in r_bits_positions:
        sm = 0
        for start in range(pos, len(s1), pos * 2):
            end = len(s1)
            if start + pos < end:
                end = start + pos
            for k in range(start, end):
                sm += s1[k]
        s1[pos] = sm % 2
    print("".join([str(i) for i in s1][1:]))

else:
    r_bits_positions = []
    s1 = []
    start = 0
    for o in range(len(s)):
        if o == 2 ** start:
            s1.append(0)
            start += 1
            r_bits_positions.append(o)
        else:
            s1.append(s[o])
    # print("".join([str(i) for i in s1][1:]) + " : нули")
    r_bits_amount = len(r_bits_positions)

    for pos in r_bits_positions:
        sm = 0
        for start in range(pos, len(s1), pos * 2):
            end = len(s1)
            if start + pos < end:
                end = start + pos
            for k in range(start, end):
                sm += s1[k]
        s1[pos] = sm % 2
    # print("".join([str(i) for i in s1][1:]) + " : заново")

    control = 0
    for pos in r_bits_positions:
        if s[pos] != s1[pos]:
            control += pos
    s[control] = abs(1 - s[control])

    res = []
    for i in range(1, len(s)):
        if i not in r_bits_positions:
            res.append(s[i])
    print("".join([str(i) for i in res]))
