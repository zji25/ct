#include <bits/stdc++.h>

using namespace std;

vector<vector<int>> adj;
vector<int> s, h, p, tin, tout, st;
int nn, t = 0;
void sz(int x) {
    s[x] = 1;
    for (int & y : adj[x]) {
        adj[y].erase(std::remove(adj[y].begin(), adj[y].end(), x), adj[y].end());
        p[y] = x; sz(y); s[x] += s[y];
        if (s[y] > s[adj[x][0]]) swap(y, adj[x][0]);
    }
}
void hld(int x) {
    tin[x] = t++;
    for (int y : adj[x]) { h[y] = (y == adj[x][0] ? h[x] : y); hld(y); }
    tout[x] = t;
}
int rtee(int x, int y, int i, int l, int r) {
    if (r < x || y < l) return 0;
    if (x <= l && r <= y) return st[i];
    int m = l + (r - l) / 2;
    return max(rtee(x, y, 2 * i + 1, l, m), rtee(x, y, 2 * i + 2, m + 1, r));
}
int anc(int x, int y) { return tin[x] <= tin[y] && tin[y] < tout[x]; }
int gg(int x, int y) {
    int w = 0;
    while (!anc(h[x], y)) w = max(w, rtee(tin[h[x]], tin[x], 0, 0, nn - 1)), x = p[h[x]];
    while (!anc(h[y], x)) w = max(w, rtee(tin[h[y]], tin[y], 0, 0, nn - 1)), y = p[h[y]];
    w = max(w, rtee(min(tin[x], tin[y]), max(tin[x], tin[y]), 0, 0, nn - 1));
    return w;
}
void ii(int x, int y, int i, int l, int r) {
    if (r < x || x < l) return;
    if (l == r) { st[i] += y; return; }
    int m = l + (r - l) / 2;
    if (x <= m) ii(x, y, 2 * i + 1, l, m);
    else ii(x, y, 2 * i + 2, m + 1, r);
    st[i] = max(st[2 * i + 1], st[2 * i + 2]);
}
int main() {
    freopen("caves.in", "r", stdin);
    freopen("caves.out", "w", stdout);
    int n, q, x, y; char c;
    cin >> n; nn = 1 << (int) ceil(log2(n)); n++;
    adj.resize(n), s.resize(n), h.resize(n), p.resize(n), tin.resize(n), tout.resize(n);
    st.resize(2 * nn - 1, 0);
    for (int i = 2; i < n; ++i) cin >> x >> y, adj[x].push_back(y), adj[y].push_back(x);
    p[1] = 1, h[1] = 1;
    sz(1); hld(1);
    cin >> q;
    while (q--) {
        cin >> c >> x >> y;
        if (c == 'G') cout << gg(x, y) << '\n';
        else ii(tin[x], y, 0, 0, nn - 1);
    }
}