#include <bits/stdc++.h>

using namespace std;

long long skiz = numeric_limits<long long>::min(), qv;
int qd;
vector<vector<int>> adj;
vector<long long> vl;
vector<bool> dead;
vector<int> zc;
struct ff { bool operator()(const int x, const int y) const { return vl[x] >= vl[y]; }};
map<int, pair<int,int>, ff> ds;

void dfs(int v, int p) {
    zc[v] = 1;
    for (int u : adj[v]) if (u != p && !dead[u]) dfs(u, v), zc[v] += zc[u];
}
void sfd(int v, int p, int d, int cc) {
    qd = d;
    ds.insert({v, {d++, cc}}), qv = max(qv, vl[v]);
    for (int u : adj[v]) if (u != p && !dead[u]) sfd(u, v, d, cc);
}
void dcmps(int v, int p) {
    dfs(v, -1);
    int xx, s = zc[v], c = -1;
    if (s < 2) return;
    while (v != -1) {
        xx = -1;
        for (int u : adj[v]) if (u != c && !dead[u] && 2 * zc[u] > s) { xx = u; break; }
        c = v, v = xx;
    }
    qv = numeric_limits<long long>::min();
    ds.insert({c, {0, -1}});
    for (int u : adj[c]) if (u != p && !(dead[u])) sfd(u, c, 1, u);
    if (qv * qd * 2 < skiz || ds.size() < 2) { ds.clear(); return; }
    bool ss = true;
    int xd, edg, yd = -1;
    for (auto & [z, w] : ds) {
        if (ss) { xd = w.first, edg = w.second; ss = false; continue; }
        if (w.second == edg) {
            if (yd != -1) skiz = max(skiz, vl[z] * (w.first + yd));
            xd = max(xd, w.first);
        } else {
            skiz = max(skiz, vl[z] * (w.first + xd));
            if (w.first > xd) yd = xd, xd = w.first, edg = w.second;
            else yd = max(yd, w.first);
        }
    }
    ds.clear();
    dead[c] = true;
    for (int x : adj[c]) if (!dead[x]) dcmps(x, c);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, x, y;
    cin >> n;
    vl.resize(n), adj.resize(n), zc.resize(n), dead.resize(n);
    for (int i = 0; i < n; ++i) cin >> vl[i];
    for (int i = 1; i < n; ++i) cin >> x >> y, --x, --y, adj[x].push_back(y), adj[y].push_back(x);
    dcmps(0, -1);
    cout << skiz;
}