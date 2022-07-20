n = int(input())
a = []


def extract(heap):
    res = heap[0]
    heap[0] = heap[-1]
    heap.pop()
    i = 0
    n = len(heap)
    while 2 * i + 1 < n:
        max_child = (heap[2 * i + 1], 2 * i + 1)
        if 2 * i + 2 < n and heap[2 * i + 2] > heap[2 * i + 1]:
            max_child = (heap[2 * i + 2], 2 * i + 2)
        if max_child[0] > heap[i]:
            heap[i], heap[max_child[1]] = heap[max_child[1]], heap[i]
            i = max_child[1]
        else:
            break
    return res


def insert(x, heap):
    heap.append(x)
    i = len(heap) - 1
    while heap[i] > heap[(i - 1) // 2] and i > 0:
        heap[i], heap[(i - 1) // 2] = heap[(i - 1) // 2], heap[i]
        i = (i - 1) // 2
    return heap


for i in range(n):
    inp = input()
    if inp[0] == '1':
        print(extract(a))
    else:
        a = insert(int(inp[2:]), a)
