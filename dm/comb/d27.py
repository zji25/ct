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
    if len(string) == 0:
        return "-"
    string += ")" + "(" * co + ")" * (cc - 1)
    return string


def mm():
    s = input()
    print(ns(s))


if __name__ == '__main__':
    mm()