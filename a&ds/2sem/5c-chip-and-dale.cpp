#include <bits/stdc++.h>

using namespace std;

bool fcc(int x, int y, int nn, vector<int> & dh, vector<vector<int>> & cz, vector<vector<int>> & wi) {
    int sw = 0;
    if (dh[x] > dh[y]) swap(x, y), sw++;
    for (int i = nn - 1; i > 0; i--) {
        if (dh[cz[y][i]] > dh[x] - 1 && wi[y][i] != 2) {
            if (wi[y][i] != sw) return false;
            y = cz[y][i];
        }
        if (x == y) return true;
    }
    for (int i = nn - 1; i > 0; i--) {
        if (wi[x][i] != 2 && wi[y][i] != 2) {
            if (wi[y][i] != sw || wi[x][i] != abs(sw - 1)) return false;
            x = cz[x][i]; y = cz[y][i];
            if (x == y) return true;
        }
    }
    return false;
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, nn, x, y, c = 1;
    cin >> n; n++; nn = log2(n) + 4;
    vector<int> dh(n, -1);
    vector<vector<int>> cz(n, vector<int>(nn));
    vector<vector<int>> wi(n, vector<int>(nn));
    vector<int> tp(n);
    cz[1][1] = 1; dh[1] = 0;
    for (int i = 2; i < n; i++) {
        cin >> x >> y;
        if (!tp[x]) tp[x] = c++;
        if (!tp[y]) tp[y] = c++;
        cz[max(tp[x], tp[y])][1] = min(tp[x], tp[y]);
        wi[max(tp[x], tp[y])][1] = (tp[x] > tp[y]);
    }
    for (int i = 2; i < n; i++) dh[i] = dh[cz[i][1]] + 1;
    for (int i = 2; i < nn; i++) {
        for (int j = 1; j < n; j++) {
            cz[j][i] = cz[cz[j][i - 1]][i - 1];
            wi[j][i] = wi[cz[j][i - 1]][i - 1] == wi[j][i - 1] ? wi[j][i - 1] : 2;
        }
    }
    cin >> m;
    while (m--) {
        cin >> x >> y;
        if (x == y) cout << "Yes\n";
        else cout << (fcc(tp[x], tp[y], nn, dh, cz, wi) ? "Yes\n" : "No\n");
    }
}