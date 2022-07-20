#include <bits/stdc++.h>

using namespace std;

long long skiz = numeric_limits<long long>::max();
void prp(vector<vector<long long>>& st, int nn, int i) { 
    if (i > nn - 2) return;
    if (st[i][1] < skiz) {
        for (int j = 1; j < 3; j++) {
            st[2 * i + j][0] = st[i][1] + st[i][2];
            st[2 * i + j][1] = st[i][1] + st[i][2];
            st[2 * i + j][2] = 0;
        }
        st[i][1] = skiz; st[i][2] = 0;
    } else {
        for (int j = 1; j < 3; j++) {
            st[2 * i + j][0] += st[i][2];
            st[2 * i + j][2] += st[i][2];
        }
        st[i][2] = 0;
    }
}
void jguh(vector<vector<long long>>& st, int nn, long long add, int l, int r, int i, int tl, int tr) {
    prp(st, nn, i);
    if (tr < l || r < tl) return;
    if (l <= tl && tr <= r) {
        st[i][0] += add; st[i][2] += add; return;
    }
    int m = tl + (tr - tl) / 2;
    jguh(st, nn, add, l, r, 2 * i + 1, tl, m);
    jguh(st, nn, add, l, r, 2 * i + 2, m + 1, tr);
    st[i][0] = min(st[2 * i + 1][0], st[2 * i + 2][0]);
}
long long rtee(vector<vector<long long>>& st, int nn, int l, int r, int i, int tl, int tr) {
    prp(st, nn, i);
    if (tr < l || r < tl) return skiz;
    if (l <= tl && tr <= r) return st[i][0];
    int m = tl + (tr - tl) / 2;
    return min(rtee(st, nn, l, r, 2 * i + 1, tl, m), rtee(st, nn, l, r, 2 * i + 2, m + 1, tr));
}
void sta(vector<vector<long long>>& st, int nn, long long stt, int l, int r, int i, int tl, int tr) {
    prp(st, nn, i);
    if (tr < l || r < tl) return;
    if (l <= tl && tr <= r) {
        st[i][0] = st[i][1] = stt; st[i][2] = 0; return;
    }
    int m = tl + (tr - tl) / 2;
    sta(st, nn, stt, l, r, 2 * i + 1, tl, m);
    sta(st, nn, stt, l, r, 2 * i + 2, m + 1, tr);
    st[i][0] = min(st[2 * i + 1][0], st[2 * i + 2][0]);
}
int main() {
    int n, nn, t, x, y;
    long long z;
    cin >> n;
    nn = 1 << (int) ceil(log2(n));
    vector<vector<long long>> st(2 * nn - 1, vector<long long>(3));
    for (int i = 0; i < 2 * nn - 1; i++) { //min set add
        st[i][0] = st[i][1] = skiz;
        st[i][2] = 0;
    }
    for (int i = 0; i < n; i++) {
        cin >> t;
        sta(st, nn, t, i, i, 0, 0, nn - 1);
    }
    string s;
    while (cin >> s) {
        if (s == "min") {
            cin >> x >> y;
            cout << rtee(st, nn, x - 1, y - 1, 0, 0, nn - 1) << '\n';
        } else if (s == "set") {
            cin >> x >> y >> z;
            sta(st, nn, z, x - 1, y - 1, 0, 0, nn - 1);
        } else {
            cin >> x >> y >> z;
            jguh(st, nn, z, x - 1, y - 1, 0, 0, nn - 1);
        } 
    }
}