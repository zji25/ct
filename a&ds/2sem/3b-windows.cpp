#include <bits/stdc++.h>

using namespace std;

struct rec { int x, yb, yt, t; rec(int x, int yb, int yt, int t) : x(x), yb(yb), yt(yt), t(t) {}};
long long skiz = numeric_limits<long long>::min();

void sxd(vector<vector<long long>>& st, int tl, int tr, int i) {
    st[i][2] = tl;
    if (tl == tr) return;
    int m = tl + (tr - tl) / 2;
    sxd(st, tl, m, i * 2 + 1);
    sxd(st, m + 1, tr, i * 2 + 2);
}
void prp(vector<vector<long long>>& st, int nn, int i) {
    if (i > nn - 2) return;
    for (int j = 1; j < 3; j++) {
        st[2 * i + j][0] += st[i][1];
        st[2 * i + j][1] += st[i][1];
    }
    st[i][1] = 0;
}
void jguh(vector<vector<long long>>& st, int nn, int add, int l, int r, int i, int tl, int tr) {
    prp(st, nn, i);
    if (tr < l || r < tl) return;
    if (l <= tl && tr <= r) {
        st[i][0] += add; st[i][1] += add;
        return;
    }
    int m = tl + (tr - tl) / 2;
    jguh(st, nn, add, l, r, 2 * i + 1, tl, m);
    jguh(st, nn, add, l, r, 2 * i + 2, m + 1, tr);
    st[i][2] = st[2 * i + 1][0] > st[2 * i + 2][0] ? st[2 * i + 1][2] : st[2 * i + 2][2];
    st[i][0] = max(st[2 * i + 1][0], st[2 * i + 2][0]);
}
vector<long long> rtee(vector<vector<long long>>& st, int nn, int l, int r, int i, int tl, int tr) {
    prp(st, nn, i);
    if (tr < l || r < tl) return {skiz};
    if (l <= tl && tr <= r) return st[i];
    int m = tl + (tr - tl) / 2;
    vector<long long> a = rtee(st, nn, l, r, 2 * i + 1, tl, m), b = rtee(st, nn, l, r, 2 * i + 2, m + 1, tr);
    return a[0] > b[0] ? a : b;
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int nn = 1 << 19, off = 1 << 18, xx = 0, n, x1, y1, x2, y2;
    vector<rec> recs;
    vector<vector<long long>> st(2 * nn - 1, vector<long long>(3));
    sxd(st, 0, nn - 1, 0);
    cin >> n;
    for (int i = 0; i < n; ++i) {
        cin >> x1 >> y1 >> x2 >> y2;
        recs.push_back(rec(x1, y1, y2, 1));
        recs.push_back(rec(x2, y1, y2, -1));
    }
    sort(recs.begin(), recs.end(), [](rec a, rec b) { return (a.x < b.x || (a.x == b.x && a.t > b.t)); });
    vector<long long> rs = {skiz}, tm;
    for (rec r : recs) {
        jguh(st, nn, r.t, r.yb + off, r.yt + off, 0, 0, nn - 1);
        tm = rtee(st, nn, 0, nn - 1, 0, 0, nn - 1);
        if (tm[0] > rs[0]) rs = tm, xx = r.x;
    }
    cout << rs[0] << '\n' << xx << ' ' << rs[2] - off;
}