#include <bits/stdc++.h>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, nn, x, y, eo;
    cin >> n; n++; nn = log2(n) + 10;
    vector<int> dh(n, -1);
    vector<vector<int>> cz(n, vector<int>(nn)), wi(n, vector<int>(nn, 1e6 + 10));
    cz[1][1] = 1; dh[1] = 0;
    for (int i = 2; i < n; i++) {
        cin >> cz[i][1] >> wi[i][1]; dh[i] = dh[cz[i][1]] + 1;
    }
    for (int i = 2; i < nn; i++) {
        for (int j = 1; j < n; j++) {
            cz[j][i] = cz[cz[j][i - 1]][i - 1];
            wi[j][i] = min(wi[cz[j][i - 1]][i - 1], wi[j][i - 1]);
        }
    }
    cin >> m;
    while (m--) {
        cin >> x >> y;
        eo = 1e6 + 10;
        if (dh[x] > dh[y]) swap(x, y);
        for (int i = nn - 1; i > 0; i--) {
            if (dh[cz[y][i]] > dh[x] - 1) {
                eo = min(eo, wi[y][i]); y = cz[y][i];
            }
        }
        if (x == y) { cout << eo << '\n'; continue; }
        for (int i = nn - 1; i > 0; i--) {
            if (cz[x][i] != cz[y][i]) {
                eo = min(min(eo, wi[x][i]), wi[y][i]);
                x = cz[x][i]; y = cz[y][i];
            }
        }
        cout << min(min(eo, wi[x][1]), wi[y][1]) << '\n';
    }
}