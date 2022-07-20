#include <bits/stdc++.h>

using namespace std;

struct nd { int v; nd* l = nullptr, *r = nullptr; nd(int v) : v(v) {}; };

void upd(nd* l, nd* r, int ll, int rr, long long k) {
    if (l) *r = *l;
    if (ll == rr) { r->v++; return; }
    int mm = ll + (rr - ll) / 2;
    if (k <= mm) { r->l = new nd(0); upd(l ? l->l : nullptr, r->l, ll, mm, k); } 
    else { r->r = new nd(0); upd(l ? l->r : nullptr, r->r, mm + 1, rr, k); }
    r->v = (r->l ? r->l->v : 0) + (r->r ? r->r->v : 0);
}
int kth(nd* l, nd* r, long long k, int ll, int rr) {
    if (ll == rr) return ll;
    int mm = ll + (rr - ll) / 2;
    if (l && l->l && r->l->v - l->l->v >= k) return kth(l->l, r->l, k, ll, mm);
    else if ((!l || !l->l) && r->l && r->l->v >= k) return kth(nullptr, r->l, k, ll, mm);
    else if (l && l->l) return kth(l->r, r->r, k - r->l->v + l->l->v, mm + 1, rr);
    else if (r->l) return kth(l ? l->r : nullptr, r->r, k - r->l->v, mm + 1, rr);
    else return kth(l ? l->r : nullptr, r->r, k, mm + 1, rr);
}
int main() {
    int l, m, n, b, g, lx, mx, ly, my, lk, mk, f = 1e9;
    long long a, x, y, i, j, k, r = 0;
    cin >> n >> a >> l >> m >> b;
    vector<nd*> rs(n + 1);
    rs[1] = new nd(0);
    upd(rs[0], rs[1], 0, f, a);
    for (int i = 1; i < n; i++) {
        rs[i + 1] = new nd(0);
        a = (a * l + m) % f;
        upd(rs[i], rs[i + 1], 0, f, a);
    }
    while (b--) {
        cin >> g >> x >> lx >> mx >> y >> ly >> my >> k >> lk >> mk;
        g--;
        i = min(x, y); j = max(x, y);
        r += kth(rs[i - 1], rs[j], k, 0, f);
        while (g--) {
            x = ((i - 1) * lx + mx) % n + 1; 
            y = ((j - 1) * ly + my) % n + 1;
            i = min(x, y); j = max(x, y);
            k = ((k - 1) * lk + mk) % (j - i + 1) + 1;
            r += kth(rs[i - 1], rs[j], k, 0, f);
        }
    }
    cout << r;
}
