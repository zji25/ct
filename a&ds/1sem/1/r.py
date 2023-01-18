import sys


def main():
    n, k = map(int, sys.stdin.readline().split())
    a = [int(q) for q in sys.stdin.readline().split()]
    b = [int(q) for q in sys.stdin.readline().split()]
    a.sort()
    b.sort()
    print(kthsum(a, b, k))


def kthsum(aa, bb, k):
    left = 2
    right = (10 ** 9) * 2 + 1
    while left < right:
        mid = (left + right) // 2
        if leq(aa, bb, mid) < k:
            left = mid + 1
        else:
            right = mid
    return left


def leq(aa, bb, tg):
    count = 0
    for aaa in aa:
        left = 0
        right = len(bb) - 1
        while left + 1 < right:
            mid = (left + right) // 2
            sm = aaa + bb[mid]
            if sm <= tg:
                left = mid
            else:
                right = mid - 1

        if aaa + bb[right] <= tg:
            count += right + 1
        elif aaa + bb[left] <= tg:
            count += left + 1
    return count


if __name__ == '__main__':
    main()
