def ns(string):
    cc = 0
    co = 0
    for j in range(len(string) - 1, -1, -1):
        if string[j] == "(":
            co += 1
            if cc > co:
                break
        else:
            cc += 1
    string = string[:(len(string) - co - cc)]
    if string == "":
        return "nope"
    string += ")" + "(" * co + ")" * (cc - 1)
    return string


def mm():
    n = int(input())
    s = "(" * n + ")" * n
    while s != "nope":
        print(s)
        s = ns(s)


if __name__ == '__main__':
    mm()