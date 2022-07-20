#include <bits/stdc++.h>

using namespace std;

int skiz = numeric_limits<int>::max(), shhh = numeric_limits<int>::min();
vector<int> t;
void sxd(int v, int tl, int tr) {
    if (tl == tr) return;
    t[v * 2 + 1] = max(t[v * 2 + 1], t[v]); t[v * 2 + 2] = max(t[v * 2 + 2], t[v]);
    int m = (tr + tl) / 2;
    sxd(2 * v + 1, tl, m); 
    sxd(2 * v + 2, m + 1, tr);
    t[v] = min(t[v * 2 + 1], t[v * 2 + 2]);
}
void upd(int v, int tl, int tr, int l, int r, int x) {
    if (tr < l || r < tl) return;
    if (l <= tl && tr <= r) {
        t[v] = max(t[v], x); return;
    }
    int m = (tr + tl) / 2;
    upd(2 * v + 1, tl, m, l, r, x); 
    upd(2 * v + 2, m + 1, tr, l, r, x);
}
int gta(int v, int tl, int tr, int l, int r) {
    if (tr < l || r < tl) return skiz;
    if (l <= tl && tr <= r) return t[v];
    int m = (tr + tl) / 2;
    return min(gta(2 * v + 1, tl, m, l, r), gta(2 * v + 2, m + 1, tr, l, r));
}
void prt(int v, int tl, int tr) {
    if (tl == tr) {
        cout << t[v] << ' '; return;
    } 
    int m = (tr + tl) / 2;
    prt(2 * v + 1, tl, m); 
    prt(2 * v + 2, m + 1, tr); 
}
int main() {
    freopen("rmq.in", "r", stdin); 
    freopen("rmq.out", "w", stdout);
    int n, m, nn; cin >> n >> m; nn = 1 << (int) ceil(log2(n));
    t.resize(2 * nn - 1, shhh);
    vector<vector<int>> qe(m, vector<int>(3));
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < 3; j++) cin >> qe[i][j];
        upd(0, 0, n - 1, qe[i][0] - 1, qe[i][1] - 1, qe[i][2]);
    }
    sxd(0, 0, n - 1);
    for (auto b : qe) {
        if (b[2] < gta(0, 0, n - 1, b[0] - 1, b[1] - 1)) {
            cout << "inconsistent\n"; return 0;
        }
    }
    cout << "consistent\n";
    prt(0, 0, n - 1);
}
