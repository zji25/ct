#include <bits/stdc++.h>
 
using namespace std;

void prp(vector<vector<int>>& st, int i, int tl, int tr) {
    int x = 2 * i + 1;
    int y = 2 * i + 2;
    int m = tl + (tr - tl) / 2;
    if (st[i][4] == 1) { 
        st[x][0] = m - tl + 1; 
        st[y][0] = tr - m;     
        st[x][1] = st[y][1] = st[x][2] = st[y][2] = st[x][3] = st[y][3] = st[x][4] = st[y][4] = 1; 
    } else if (st[i][4] == 2) {
        st[x][0] = st[y][0] = st[x][1] = st[y][1] = st[x][2] = st[y][2] = st[x][3] = st[y][3] = 0; 
        st[x][4] = st[y][4] = 2; 
    }
    st[i][4] = 0; 
}
vector<int> jgie(vector<int> x, vector<int> y) {
    x[0] += y[0]; 
    if (x[3] == y[2] && y[2] == 1) x[1] += y[1] - 1; 
    else x[1] += y[1]; 
    x[3] = y[3]; x[4] = 0; 
    return x;
} 
void fck(vector<vector<int>>& st, int cet, int l, int r, int tl, int tr, int i) {
    if (tr < l || r < tl) return;
    if (l <= tl && tr <= r) {
        if (cet) {
            st[i][0] = tr - tl + 1; 
            st[i][1] = st[i][2] = st[i][3] = st[i][4] = 1;
        } else {
            st[i][0] = st[i][1] = st[i][2] = st[i][3] = 0; 
            st[i][4] = 2;
        }
        return;
    }
    int m = tl + (tr - tl) / 2;
    prp(st, i, tl, tr);
    fck(st, cet, l, r, tl, m, 2 * i + 1);
    fck(st, cet, l, r, m + 1, tr, 2 * i + 2);
    st[i] = jgie(st[2 * i + 1], st[2 * i + 2]);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n = 1 << 20, nn = 1 << 19, t, x, l, cet;
    cin >> t;
    vector<vector<int>> st(2 * n - 1, vector<int>(5)); 
    string c;
    while (t--) {
        cin >> c >> x >> l;
        cet = c == "W" ? 0 : 1; 
        fck(st, cet, x - 1 + nn, x + l - 2 + nn, 0, n - 1, 0);
        cout << st[0][1] << ' ' << st[0][0] << '\n';
    }
}
