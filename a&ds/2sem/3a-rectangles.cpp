#include <bits/stdc++.h>

using namespace std;

#define mx(a, b, c, d) max(max(a, b), max(c, d))
#define mn(a, b, c, d) min(min(a, b), min(c, d))
#define mxn(a, b) { max(a[0], b[0]), min(a[1], b[1]), max(a[2], b[2]), min(a[3], b[3]) }
#define sd vector<int> &

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, x1, y1, x2, y2, q, a, b, xk, yk, e0, e1, e2, e3, eps = 1e6, mx = 1e9 + 7, kj[130];
    long long v, r1, c1, r2, c2, xi, xj, yi, yj, t, eo = 0;
    
    kj[0] = kj[1] = 0;
    for (int i = 2; i < 130; i++) kj[i] = kj[i / 2] + 1;
    cin >> n >> m;
    vector<vector<vector<vector<vector<int>>>>> juh(n, vector<vector<vector<vector<int>>>>(m,
    vector<vector<vector<int>>>(kj[n] + 2, vector<vector<int>>(kj[m] + 2, {-eps, eps, -eps, eps}))));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cin >> x1 >> y1 >> x2 >> y2;
            juh[i][j][0][0] = { min(x1, x2), max(x1, x2), min(y1, y2), max(y1, y2) };
        }
    }
    for (int i = 0; i < n; ++i) {
        for (int k = 1; (1 << k) <= m; ++k) {
            for (int j = 0; j <= m - (1 << k); ++j) {
                juh[i][j][0][k] = mxn(juh[i][j][0][k - 1], juh[i][j + (1 << (k - 1))][0][k - 1]);
            }
        }
    }
    for (int k = 1; (1 << k) <= n; ++k) {
        for (int i = 0; i <= n - (1 << k); ++i) {
            for (int l = 0; l < kj[m] + 2; ++l) {
                for (int j = 0; j < m; ++j) {
                    juh[i][j][k][l] = mxn(juh[i][j][k - 1][l], juh[i + (1 << (k - 1))][j][k - 1][l]);
                }
            }
        }
    }
    cin >> q >> a >> b >> v;
    while (q--) {
        v = (a * v + b) % mx; r1 = v % n; v = (a * v + b) % mx; c1 = v % m;
        v = (a * v + b) % mx; r2 = v % n; v = (a * v + b) % mx; c2 = v % m;
        xi = min(r1, r2), xj = max(r1, r2), yi = min(c1, c2), yj = max(c1, c2);
        xk = kj[abs(r1 - r2) + 1], yk = kj[abs(c1 - c2) + 1];

        sd aa = juh[xi][yi][xk][yk];
        sd bb = juh[xj - (1 << xk) + 1][yi][xk][yk];
        sd cc = juh[xi][yj - (1 << yk) + 1][xk][yk];
        sd dd = juh[xj - (1 << xk) + 1][yj - (1 << yk) + 1][xk][yk];

        e0 = mx(aa[0], bb[0], cc[0], dd[0]), e1 = mn(aa[1], bb[1], cc[1], dd[1]),
        e2 = mx(aa[2], bb[2], cc[2], dd[2]), e3 = mn(aa[3], bb[3], cc[3], dd[3]);
        t = (e0 >= e1 || e2 >= e3) ? 0 : (static_cast<long long>(e1 - e0) * (e3 - e2)) % mx;
        eo = (eo + t) % mx;
    }
    cout << eo;
}
