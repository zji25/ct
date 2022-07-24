#include <bits/stdc++.h>

using namespace std;

int skiz = numeric_limits<int>::max();
vector<vector<pair<int,int>>> cp;
vector<vector<int>> adj;
vector<unordered_map<int, int>> cd, ds;
vector<int> zc; vector<bool> dead;

void dfs(int v, int p) {
    zc[v] = 1;
    for (int u : adj[v]) if (u != p && !dead[u]) dfs(u, v), zc[v] += zc[u];
}
void sfd(int c, int v, int p, int d) {
    if (c != v) ds[c].emplace(v, d);
    for (int u : adj[v]) if (u != p && !dead[u]) sfd(c, u, v, d + 1);
}
void dcmps(int v, int p) {
    dfs(v, -1);
    int xx, s = zc[v], c = -1;
    while (v != -1) {
        xx = -1;
        for (int u : adj[v]) if (u != c && !dead[u] && 2 * zc[u] > s) { xx = u; break; }
        c = v, v = xx;
    }
    sfd(c, c, p, 0);
    dead[c] = true;
    cp[c].push_back({p, p == -1 ? 0 : ds[p][c]});
    if (p != -1) for (auto [xc, _] : cp[p]) if (xc != -1) cp[c].push_back({xc, ds[xc][c]});
    for (int x : adj[c]) if (!dead[x]) dcmps(x, c);
    for (auto [z, w] : cp[c]) if (z != -1) {
        for (auto [x, y]: cd[c]) {
            auto itt = cd[z].find(x);
            if (itt == cd[z].end() || y + w < itt->second) cd[z].insert_or_assign(x, y + w);
        }
    }
}
int zxc(int x, int y) {
    int q = skiz;
    auto it = cd[x].find(y);
    if (it != cd[x].end()) q = it->second;
    if (!q) return 0;
    for (auto [z, w] : cp[x]) if (z != -1) {
        it = cd[z].find(y);
        if (it != cd[z].end()) { q = min(q, it->second + w); if (!q) return 0; }
    }
    return q == skiz ? -1 : q;
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, q, x, y;
    cin >> n;
    adj.resize(n), zc.resize(n), cp.resize(n), dead.resize(n), cd.resize(n), ds.resize(n);
    for (int i = 1; i < n; ++i) cin >> x, adj[i].push_back(x), adj[x].push_back(i);
    for (int i = 0; i < n; ++i) cin >> x, cd[i].insert({x, 0});
    dcmps(0, -1);
    cin >> q;
    while (q--) cin >> x >> y, cout << zxc(x, y) << ' ';
}