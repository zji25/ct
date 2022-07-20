#include <bits/stdc++.h>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, nn, x, y;
    cin >> n; n++; nn = log(n) + 10;
    vector<int> dh(n, -1);
    vector<vector<int>> cz(n, vector<int>(nn));
    cz[1][1] = 1; dh[1] = 0;
    for (int i = 2; i < n; i++) {
        cin >> cz[i][1]; dh[i] = dh[cz[i][1]] + 1;
    }
    for (int i = 2; i < nn; i++) {
        for (int j = 1; j < n; j++) {
            cz[j][i] = cz[cz[j][i - 1]][i - 1];
        }
    }
    cin >> m;
    while (m--) {
        cin >> x >> y;
        if (dh[x] > dh[y]) swap(x, y);
        for (int i = nn - 1; i > 0; i--) {
            if (dh[cz[y][i]] > dh[x] - 1) y = cz[y][i];
        }
        if (x == y) { cout << x << '\n'; continue; }
        for (int i = nn - 1; i > 0; i--) {
            if (cz[x][i] != cz[y][i]) {
                x = cz[x][i]; y = cz[y][i];
            }
        }
        cout << cz[x][1] << '\n';
    }
}
