s = input()
st = set()
res = []
for i in s:
    st.add(i)
st = sorted(list(st))
for i in s:
    res.append(st.index(i) + 1)
    t = st.pop(st.index(i))
    st.insert(0, t)
print(" ".join([str(i) for i in res]))
