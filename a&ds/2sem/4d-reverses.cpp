#include <bits/stdc++.h>

using namespace std;

struct nd { int v, p, sz = 1; nd* l = nullptr; nd* r = nullptr; bool rv = false;
    nd(int v): v(v), p(rand()){}};

nd* tr = nullptr;
int sz(nd* x) { return x ? x->sz : 0; }
void rcl(nd* x) { x->sz = sz(x->l) + sz(x->r) + 1; }
void ous(nd* x) {
    if (!x || !x->rv) return;
    swap(x->l, x->r);
    x->rv = false;
    if (x->l) x->l->rv ^= true;
    if (x->r) x->r->rv ^= true;
}
void prt(nd * x) { 
    if (!x) return; ous(x);
    prt(x->l); cout << x->v << ' '; prt(x->r); 
}
pair<nd*, nd*> split(nd* x, int k) {
    if (!x) return make_pair(nullptr, nullptr);
    ous(x);
    pair<nd*, nd*> t;
    if (k <= sz(x->l)) { t = split(x->l, k); x->l = t.second; rcl(x); return make_pair(t.first, x); } 
    t = split(x->r, k - sz(x->l) - 1); x->r = t.first; rcl(x); return make_pair(x, t.second);
}
nd* merge(nd* a, nd* b) {
    ous(a); ous(b); if (!a) return b; if (!b) return a;
    if (a->p < b->p) { a->r = merge(a->r, b); rcl(a); return a; }
    b->l = merge(a, b->l); rcl(b); return b;
}
nd* addd(nd* x, int k) {
    pair<nd*, nd*> t = split(x, k);
    t.first = merge(t.first, new nd(k));
    return merge(t.first, t.second);
}
nd* sqa(nd* x, int a, int b) {
    pair<nd*, nd*> lr = split(x, a - 1);
    pair<nd*, nd*> mr = split(lr.second, b - a + 1);
    if (mr.first) mr.first->rv ^= true;
    return merge(merge(lr.first, mr.first), mr.second);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, l, r; 
    cin >> n >> m; 
    for (int i = 1; i < n + 1; i++) tr = addd(tr, i);
    while (m--) {
        cin >> l >> r;
        tr = sqa(tr, l, r);
    }
    prt(tr);
}