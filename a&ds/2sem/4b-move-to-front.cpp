#include <bits/stdc++.h>

using namespace std;

struct nd { int v, p, sz = 1; nd* l = nullptr; nd* r = nullptr;
    nd(int v): v(v), p(rand()) {}};

nd* tr = nullptr;
int sz(nd* x) { return x ? x->sz : 0; }
void recalc(nd* x) { x->sz = sz(x->l) + sz(x->r) + 1; }
void prnt(nd * x) { if (!x) return; prnt(x->l); cout << x->v << ' '; prnt(x->r); }

pair<nd*, nd*> split(nd* x, int k) {
    if (!x) return make_pair(nullptr, nullptr);
    pair<nd*, nd*> t;
    if (k <= sz(x->l)) { t = split(x->l, k); x->l = t.second; recalc(x); return make_pair(t.first, x); } 
    t = split(x->r, k - sz(x->l) - 1); x->r = t.first; recalc(x); return make_pair(x, t.second);
}
nd* merge(nd* a, nd* b) {
    if (!a) return b; 
    if (!b) return a;
    if (a->p < b->p) { a->r = merge(a->r, b); recalc(a); return a; }
    b->l = merge(a, b->l); recalc(b); return b;
}
nd* addd(nd* x, int k) {
    pair<nd*, nd*> t = split(x, k);
    t.first = merge(t.first, new nd(k));
    return merge(t.first, t.second);
}
nd* dell(int v) {
    pair<nd*, nd*> a = split(tr, v), b = split(a.second, v + 1);
    return merge(a.first, b.second);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, l, r; 
    cin >> n >> m;
    for (int i = 1; i < n + 1; i++) tr = addd(tr, i);
    while (m--) {
        cin >> l >> r;
        pair<nd*, nd*> a = split(tr, l - 1), b = split(a.second, r - l + 1);
        tr = merge(merge(b.first, a.first), b.second);
    }
    prnt(tr);
}