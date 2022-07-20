#include <bits/stdc++.h>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, u, v, ri, aru;
    cin >> n >> m;
    vector<int> a(n);
    vector<vector<int>> st(n, vector<int>(log2(n) + 5));
    cin >> a[0] >> u >> v;
    st[0][0] = a[0];
    for (int i = 1; i < n; i++) {
        a[i] = (23 * a[i - 1] + 21563) % 16714589;
        st[i][0] = a[i];
    }
    int lkj[n + 5];
    lkj[0] = lkj[1] = 0;
    for (int i = 2; i < n + 1; i++) lkj[i] = lkj[i / 2] + 1;
    for (int i = 1; (1 << i) <= n; i++) {
        for (int j = 0; j <= n - (1 << i); j++) {
            st[j][i] = min(st[j][i - 1], st[j + (1 << (i - 1))][i - 1]);    
        }
    }
    aru = lkj[abs(u - v) + 1];
    ri = min(st[min(u, v) - 1][aru], st[max(u, v) - (1 << aru)][aru]);
    for (int i = 1; i < m; i++) {
        u = (17 * u + 751 + ri + 2 * i) % n + 1;
        v = (13 * v + 593 + ri + 5 * i) % n + 1;
        aru = lkj[abs(u - v) + 1];
        ri = min(st[min(u, v) - 1][aru], st[max(u, v) - (1 << aru)][aru]);
    }
    cout << u << ' ' << v << ' ' << ri;
}
