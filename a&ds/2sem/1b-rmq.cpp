#include <bits/stdc++.h>
 
using namespace std;

long long sxd(long long* a, long long* st, int tl, int tr, int i) {
    if (tl == tr) return st[i] = a[tl];
    int m = tl + (tr - tl) / 2;
    st[i] = min(sxd(a, st, tl, m, i * 2 + 1), sxd(a, st, m + 1, tr, i * 2 + 2));
    return st[i];
}
void sta(long long* a, long long* st, int tl, int tr, int q, long long uh, int i) {
    if (q < tl || q > tr) return;
    if (tl == tr) {  
        a[q] = st[i] = uh; return;
    }
    int m = tl + (tr - tl) / 2;   
    if (q >= tl && q <= m) sta(a, st, tl, m, q, uh, 2 * i + 1);
    else sta(a, st, m + 1, tr, q, uh, 2 * i + 2); 
    st[i] = min(st[2 * i + 1], st[2 * i + 2]);
    
}
long long fck(long long* st, int tl, int tr, int l, int r, int i) {
    if (tr < l || tl > r) return numeric_limits<long long>::max();;
    if (l <= tl && r >= tr) return st[i];
    int m = tl + (tr - tl) / 2; 
    return min(fck(st, tl, m, l, r, 2 * i + 1), fck(st, m + 1, tr, l, r, 2 * i + 2));
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, aa, bb, xx;
    long long yy;
    cin >> n;
    long long a[n], st[4 * n];
    for (int i = 0; i < n; i++) cin >> a[i];
    sxd(a, st, 0, n - 1, 0);
    string s;
    while (cin >> s) {
        if (s == "set") {
            cin >> xx >> yy;
            sta(a, st, 0, n - 1, xx - 1, yy, 0);
        } else {
            cin >> aa >> bb;
            cout << fck(st, 0, n - 1, aa - 1, bb - 1, 0) << "\n";
        }
    }
}