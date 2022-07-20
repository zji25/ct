#include <bits/stdc++.h>

using namespace std;

vector<vector<int>> adj, st;
vector<int> s, h, p, tin, tout;
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
void sxd(int i, int d) {
    if (i > nn - 2 || !st[i][1]) return;
    st[2 * i + 1][0] += st[i][1] * d; st[2 * i + 1][1] += st[i][1];
    st[2 * i + 2][0] += st[i][1] * d; st[2 * i + 2][1] += st[i][1];
    st[i][1] = 0;
}
int rtee(int x, int y, int i, int l, int r) {
    sxd(i, (r - l) / 2 + 1);
    if (r < x || y < l) return 0;
    if (x <= l && r <= y) return st[i][0];
    int m = l + (r - l) / 2;
    return rtee(x, y, 2 * i + 1, l, m) + rtee(x, y, 2 * i + 2, m + 1, r);
}
int anc(int x, int y) { return tin[x] <= tin[y] && tin[y] < tout[x]; }
void jguh(int x, int y, int i, int l, int r) {
    sxd(i, (r - l) / 2 + 1);
    if (r < x || y < l) return;
    if (x <= l && r <= y) {
        st[i][0] += r - l + 1, st[i][1]++;
        return;
    }
    int m = l + (r - l) / 2;
    jguh(x, y, 2 * i + 1, l, m);
    jguh(x, y, 2 * i + 2, m + 1, r);
    st[i][0] = st[2 * i + 1][0] + st[2 * i + 2][0];
}
int bessy(int x, int y) {
    if (x == y) return 0;
    int w = 0;
    while (!anc(h[x], y)) w += rtee(tin[h[x]], tin[x], 0, 0, nn - 1), x = p[h[x]];
    while (!anc(h[y], x)) w += rtee(tin[h[y]], tin[y], 0, 0, nn - 1), y = p[h[y]];
    if (x != y) w += rtee(min(tin[x], tin[y]) + 1, max(tin[x], tin[y]), 0, 0, nn - 1);
    return w;
}
void john(int x, int y) {
    if (x == y) return;
    while (!anc(h[x], y)) jguh(tin[h[x]], tin[x], 0, 0, nn - 1), x = p[h[x]];
    while (!anc(h[y], x)) jguh(tin[h[y]], tin[y], 0, 0, nn - 1), y = p[h[y]];
    if (x != y) jguh(min(tin[x], tin[y]) + 1, max(tin[x], tin[y]), 0, 0, nn - 1);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, x, y; char c;
    cin >> n >> m; nn = 1 << (int) ceil(log2(n)); n++;
    adj.resize(n), s.resize(n), h.resize(n), p.resize(n), tin.resize(n), tout.resize(n);
    st.resize(2 * nn - 1, {0, 0});
    for (int i = 2; i < n; ++i) cin >> x >> y, adj[x].push_back(y), adj[y].push_back(x);
    p[1] = 1, h[1] = 1;
    sz(1); hld(1);
    while (m--) {
        cin >> c >> x >> y;
        if (c == 'Q') cout << bessy(x, y) << '\n';
        else john(x, y);
    }
}