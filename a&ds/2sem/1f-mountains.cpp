#include <bits/stdc++.h>

using namespace std;

struct nd { int i, rt, st, jg; nd* l = nullptr; nd* r = nullptr;
    nd(int i, int rt, int st, int jg): i(i), rt(rt), st(st), jg(jg) {}};

int skiz = numeric_limits<int>::min(), nn;
void sxd(nd* x, int l, int r) {
    if (x->i > nn - 2) return;
    if (!x->l) x->l = new nd(2 * x->i + 1, 0, skiz, 0);
    if (!x->r) x->r = new nd(2 * x->i + 2, 0, skiz, 0);
    if (x->st == skiz) return;
    x->l->rt = x->l->jg = x->r->rt = x->r->jg = ((r - l) / 2 + 1) * x->st;
    x->l->st = x->r->st = x->st;
    x->st = skiz;
}
void sta(nd* x, int d, int a, int b, int l, int r) {
    sxd(x, l, r);
    if (r < a || b < l) return;
    if (a <= l && r <= b) {
        x->rt = x->jg = (r - l + 1) * d;
        x->st = d; return;
    }
    int m = l + (r - l) / 2;
    sta(x->l, d, a, b, l, m); sta(x->r, d, a, b, m + 1, r);
    x->rt = max(x->l->rt, x->r->rt + x->l->jg);
    x->st = skiz;
    x->jg = x->l->jg + x->r->jg;
}
int rtee(nd* x, int h, int l, int r) {
    sxd(x, l, r);
    if (l == r) return l;
    int m = l + (r - l) / 2;
    return (x->l->rt > h) ? rtee(x->l, h, l, m) : rtee(x->r, h - x->l->jg, m + 1, r);
}
int main() {
    ios_base::sync_with_stdio(false); cin.tie(0); cout.tie(0);
    int n, a, b, d, h; char c;
    cin >> n; nn = 1 << (int) ceil(log2(n + 1));
    nd* xx = new nd(0, 0, skiz, 0);
    while (cin >> c) {
        switch (c) {
            case 'I': cin >> a >> b >> d; sta(xx, d, a - 1, b - 1, 0, nn - 1); break;
            case 'Q': cin >> h; cout << min(n, rtee(xx, h, 0, nn - 1)) << '\n'; break;
            case 'E': return 0;
        }
    }
}
