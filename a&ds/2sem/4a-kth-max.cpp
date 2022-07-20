#include <bits/stdc++.h>

using namespace std;

struct nd { int v, p, sz = 1; nd* l = nullptr; nd* r = nullptr;
    nd(int v): v(v), p(rand()) {}};

nd* tr = nullptr;
int sz(nd* x) { return x ? x->sz : 0; }
void recalc(nd* x) { x->sz = sz(x->l) + sz(x->r) + 1; }

pair<nd*, nd*> split(nd * x, int k) {
    if (!x) return make_pair(nullptr, nullptr);
    pair<nd*, nd*> t;
    if (k <= x->v) { t = split(x->l, k); x->l = t.second; recalc(x); return make_pair(t.first, x); } 
    t = split(x->r, k); x->r = t.first; recalc(x); return make_pair(x, t.second);
}
nd* merge(nd* a, nd* b) {
    if (!a) return b; if (!b) return a;
    if (a->p < b->p) { a->r = merge(a->r, b); recalc(a); return a; } 
    b->l = merge(a, b->l); recalc(b); return b;
}
nd* addd(nd* x, int k) {
    pair<nd*, nd*> t = split(x, k);
    t.first = merge(t.first, new nd(k));
    return merge(t.first, t.second);
}
int kth(nd* x, int k) {
    if (sz(x->r) == k - 1) return x->v;
    if (!x->r) return kth(x->l, k - 1);
    if (x->r->sz < k) return kth(x->l, k - x->r->sz - 1);
    return kth(x->r, k);
}
nd* dell(int v) {
    pair<nd*, nd*> a = split(tr, v), b = split(a.second, v + 1);
    return merge(a.first, b.second);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, c, k; 
    cin >> n; 
    while (n--) {
        cin >> c >> k;
        switch (c) {
            case 1: tr = addd(tr, k); break;
            case 0: cout << kth(tr, k) << '\n'; break;
            case -1: tr = dell(k); break;
        }
    }
}
