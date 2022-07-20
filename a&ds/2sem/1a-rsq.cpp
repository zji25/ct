#include <bits/stdc++.h>

using namespace std;

long long sxd(long long* a, long long* st, int tl, int tr, int i) {
    if (tl == tr) return st[i] = a[tl];
    int m = tl + (tr - tl) / 2;
    return st[i] = sxd(a, st, tl, m, i * 2 + 1) + sxd(a, st, m + 1, tr, i * 2 + 2);
}
void sta(long long* st, int tl, int tr, int q, long long add, int i) {
    if (q < tl || q > tr) return;
    st[i] += add;
    if (tl >= tr) return;
    int m = tl + (tr - tl) / 2;
    sta(st, tl, m, q, add, 2 * i + 1);
    sta(st, m + 1, tr, q, add, 2 * i + 2);
}
long long qpu(long long* st, int tl, int tr, int l, int r, int i) {
    if (tr < l || tl > r) return 0;
    if (l <= tl && r >= tr) return st[i];
    int m = tl + (tr - tl) / 2;
    return qpu(st, tl, m, l, r, 2 * i + 1) + qpu(st, m + 1, tr, l, r, 2 * i + 2);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, aa, bb, xx;
    long long add, yy;
    cin >> n;
    long long a[n], st[4 * n];
    for (int i = 0; i < n; i++) cin >> a[i];
    sxd(a, st, 0, n - 1, 0);
    string s;
    while (cin >> s) {
        if (s == "set") {
            cin >> xx >> yy;
            add = yy - a[xx - 1];
            a[xx - 1] = yy;
            sta(st, 0, n - 1, xx - 1, add, 0);
        } else {
            cin >> aa >> bb;
            cout << qpu(st, 0, n - 1, aa - 1, bb - 1, 0) << '\n';
        }
    }
}