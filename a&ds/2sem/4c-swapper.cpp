#include <bits/stdc++.h>

using namespace std;

struct nd { int v, p, sz = 1; long long vv = 0; nd* l = nullptr; nd* r = nullptr;
    nd(int v): v(v), vv(v), p(rand()) {}};
nd* j = nullptr; nd* q = nullptr;
int sz(nd* x) { return x ? x->sz : 0; }
long long vv(nd* x) { return x ? x->vv : 0; }
void rcl(nd* x) { if (!x) return; x->sz = sz(x->l) + sz(x->r) + 1; x->vv = vv(x->l) + vv(x->r) + x->v; }

pair<nd*, nd*> split(nd* x, int k) {
    if (!x) return make_pair(nullptr, nullptr);
    pair<nd*, nd*> t;
    if (k <= sz(x->l)) { t = split(x->l, k); x->l = t.second; rcl(x); return make_pair(t.first, x); }
    t = split(x->r, k - sz(x->l) - 1); x->r = t.first; rcl(x); return make_pair(x, t.second);
}
nd* merge(nd* a, nd* b) {
    rcl(a); rcl(b);
    if (!a) return b; if (!b) return a;
    if (a->p < b->p) { a->r = merge(a->r, b); rcl(a); return a; }
    b->l = merge(a, b->l); rcl(b); return b;
}
void swp(int x, int y) {
    pair<nd*, nd*> jlr, jmr, qlr, qmr;
    jlr = split(j, x / 2); jmr = split(jlr.second, (y - 1) / 2 - x / 2 + 1);
    qlr = split(q, (x - 1) / 2); qmr = split(qlr.second, y / 2 - (x - 1) / 2);
    j = merge(merge(jlr.first, qmr.first), jmr.second);
    q = merge(merge(qlr.first, jmr.first), qmr.second);
}
long long smm(int x, int y) {
    pair<nd*, nd*> jlr, jmr, qlr, qmr;
    jlr = split(j, x / 2); jmr = split(jlr.second, (y - 1) / 2 - x / 2 + 1);
    qlr = split(q, (x - 1) / 2); qmr = split(qlr.second, y / 2 - (x - 1) / 2);
    long long ss = vv(jmr.first) + vv(qmr.first);
    j = merge(merge(jlr.first, jmr.first), jmr.second);
    q = merge(merge(qlr.first, qmr.first), qmr.second);
    return ss;
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, m, z, t, x, y, c = 1;
    pair<nd*, nd*> f;
    while (cin >> n >> m) {
        if (n == 0 && m == 0) return 0;
        cout << "Swapper " << c << ':' << '\n';
        for (int i = 1; i <= n; i++) {
            cin >> z;
            i % 2 ? j = merge(j, new nd(z)) : q = merge(q, new nd(z));
        }
        while (m--) {
            cin >> t >> x >> y;
            if (t == 2) cout << smm(x, y) << '\n';
            else swp(x, y);
        }
        cout << '\n';
        j = nullptr; q = nullptr;
        c++;
    }
}
