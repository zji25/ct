#include <iostream>
#include <unordered_map>
#include <vector>

struct expr {
    virtual bool equals(expr* e, bool sym) = 0;
    virtual ~expr() = default;
};
std::unordered_map<std::string, expr*> vars;
struct var : public expr {
    std::string _name;
    explicit var(std::string &name) : _name(name) {}
    bool equals(expr* e, bool sym) override {
        auto* ev = dynamic_cast<var*>(e);
        if (sym) {
            if (ev == nullptr || ev->_name != _name) return false;
            vars[_name] = e;
            return true;
        }
        if (vars.find(_name) != vars.end()) {
            auto* fv = dynamic_cast<var*>(vars[_name]);
            if (fv != nullptr) return ev != nullptr && ev->_name == fv->_name;
            return vars[_name]->equals(e, true);
        }
        vars[_name] = e;
        return true;
    }
};
struct impl : public expr {
    expr* _left; expr* _right;
    impl(expr* left, expr* right) : _left(left), _right(right) {}
    bool equals(expr* e, bool sym) override {
        auto* i = dynamic_cast<impl*>(e);
        if (i == nullptr) return false;
        return _left->equals(i->_left, sym) && _right->equals(i->_right, sym);
    }
};
struct disj : public expr {
    expr* _left; expr* _right;
    disj(expr* left, expr* right) : _left(left), _right(right) {}
    bool equals(expr* e, bool sym) override {
        auto* d = dynamic_cast<disj*>(e);
        if (d == nullptr) return false;
        return _left->equals(d->_left, sym) && _right->equals(d->_right, sym);
    }
};
struct conj : public expr {
    expr* _left; expr* _right;
    conj(expr* left, expr* right) : _left(left), _right(right) {}
    bool equals(expr* e, bool sym) override {
        auto* c = dynamic_cast<conj*>(e);
        if (c == nullptr) return false;
        return _left->equals(c->_left, sym) && _right->equals(c->_right, sym);
    }
};
struct neg : public expr {
    expr* _expr;
    neg(expr* expr) : _expr(expr) {}
    bool equals(expr* e, bool sym) override {
        auto* n = dynamic_cast<neg*>(e);
        if (n == nullptr) return false;
        return _expr->equals(n->_expr, sym);
    }
};

std::string A = "a", B = "b", C = "c";
var *a = new var(A), *b = new var(B), *c = new var(C);
std::vector<expr*> axioms {
        new impl(a, new impl(b, a)),
        new impl(new impl(a, b), new impl(new impl(a, new impl(b, c)), new impl(a, c))),
        new impl(a, new impl(b, new conj(a, b))),
        new impl(new conj(a, b), a), new impl(new conj(a, b), b),
        new impl(a, new disj(a, b)), new impl(b, new disj(a, b)),
        new impl(new impl(a, c), new impl(new impl(b, c), new impl(new disj(a, b), c))),
        new impl(new impl(a, b), new impl(new impl(a, new neg(b)), new neg(a))),
        new impl(new neg(new neg(a)), a)
};
int axiom(expr* expr) {
    for (int i = 0; i < axioms.size(); ++i) { vars.clear(); if (axioms[i]->equals(expr, false)) return i+1; }
    return 0;
}
std::string input;
int inp_length, pos;
bool skip(const std::string& s) {
    while (pos<inp_length && isspace(input.at(pos))) ++pos;
    int i=pos;
    for (int j=0; j<s.length(); ++i,++j) if (inp_length<=i || input.at(i)!=s.at(j)) return false;
    pos=i;
    return true;
}
expr* expression();
expr* variable() {
    std::string x;
    while (pos<inp_length && isspace(input.at(pos))) ++pos;
    while (pos<inp_length && (int(input.at(pos))==39
        || '0'<=input.at(pos) && input.at(pos)<='9' || 'A'<=input.at(pos) && input.at(pos)<='Z')) {
        x+=input.at(pos++);
    }
    if (x.empty()) return nullptr;
    return new var(x);
}
expr* negation() {
    if (skip("(")) { auto* x = expression(); skip(")"); return x; }
    if (skip("!")) return new neg(negation());
    return variable();
}
expr* conjunction() {
    auto* x = negation();
    if (x == nullptr) return x;
    while (skip("&")) x = new conj(x, negation());
    return x;
}
expr* disjunction() {
    auto* x = conjunction();
    if (x == nullptr) return x;
    while (skip("|")) {
        if (input.at(pos) == '-') { --pos; break; }
        else x = new disj(x, conjunction());
    }
    return x;
}
expr* expression() {
    auto* x = disjunction();
    if (x == nullptr) return x;
    if (skip("->")) return new impl(x, expression());
    return x;
}
std::vector<expr*> context() {
    std::vector<expr*> cntxt;
    while (true) {
        auto* e = expression();
        if (e != nullptr) cntxt.push_back(e);
        if (skip(",")) continue;
        if (skip("|-")) break;
    }
    return cntxt;
}
int hyp(std::vector<expr*>& cntxt, expr* e) {
    for (int i = 0; i < cntxt.size(); ++i) { vars.clear(); if (cntxt[i]->equals(e, true)) return i+1; }
    return 0;
}
std::vector<std::pair<std::vector<expr*>,expr*>> mpp, ded;
std::pair<int,int> mp() {
    auto cntxt = mpp[mpp.size()-1].first;
    auto e = mpp[mpp.size()-1].second;
    for (int i = 0; i < mpp.size()-1; ++i) {
        auto* ab = dynamic_cast<impl*>(mpp[i].second);
        if (ab == nullptr || !ab->_right->equals(e, true)) continue;
        bool F = cntxt.size() == mpp[i].first.size();
        if (!F) continue;
        std::vector<bool> dead(mpp[i].first.size(), false);
        for (auto* h : cntxt) {
            bool f = false;
            for (int k = 0; k < mpp[i].first.size(); ++k) if (!dead[k]) {
                if (h->equals(mpp[i].first[k], true)) { dead[k] = true, f = true; break; }
            }
            if (!f) { F = false; break; }
        }
        if (!F) continue;
        for (int j = 0; j < mpp.size()-1; ++j) if (i!=j) {
            if (!ab->_left->equals(mpp[j].second, true)) continue;
            bool FF = cntxt.size() == mpp[j].first.size();
            if (!FF) continue;
            std::vector<bool> deadd(mpp[j].first.size(), false);
            for (auto* h : cntxt) {
                bool f = false;
                for (int k = 0; k < mpp[j].first.size(); ++k) if (!deadd[k]) {
                    if (h->equals(mpp[j].first[k], true)) { deadd[k] = true, f = true; break; }
                }
                if (!f) { FF = false; break; }
            }
            if (FF) return std::make_pair(j+1, i+1);
        }
    }
    return std::make_pair(0, 0);
}
std::pair<std::vector<expr*>, expr*> conv(std::vector<expr*> cntxt, expr* e) {
    while (true) {
        auto* i = dynamic_cast<impl*>(e);
        if (i == nullptr) break;
        cntxt.push_back(i->_left);
        e = i->_right;
    }
    return std::make_pair(cntxt, e);
}
int dd() {
    auto cntxt = ded[ded.size()-1].first;
    auto e = ded[ded.size()-1].second;
    for (int i = 0; i < ded.size()-1; ++i) {
        if (!e->equals(ded[i].second, true)) continue;
        bool F = cntxt.size() == ded[i].first.size();
        if (!F) continue;
        std::vector<bool> dead(ded[i].first.size(), false);
        for (auto* h : cntxt) {
            bool f = false;
            for (int k = 0; k < ded[i].first.size(); ++k) if (!dead[k]) {
                if (h->equals(ded[i].first[k], true)) { dead[k] = true, f = true; break; }
            }
            if (!f) { F = false; break; }
        }
        if (F) return i+1;
    }
    return 0;
}
void ct(int num, const std::string& out) {
    std::cout << "[" << num << "] " << input << " [" << out << "]\n";
}
int main() {
    int cc = 0;
    while (std::getline(std::cin, input)) {
        if (input.length() < 2) break;
        inp_length = input.length(), pos = 0, ++cc;
        std::vector<expr*> cntxt = context();
        expr* e = expression();
        mpp.push_back(std::make_pair(cntxt, e));
        ded.push_back(conv(cntxt, e));
        int ax = axiom(e);
        if (ax > 0) { ct(cc, "Ax. sch. " + std::to_string(ax)); continue; }
        int h = hyp(cntxt, e);
        if (h > 0) { ct(cc, "Hyp. " + std::to_string(h)); continue; }
        int d = dd();
        if (d > 0) { ct(cc, "Ded. " + std::to_string(d)); continue; }
        auto md = mp();
        if (md.first > 0) { ct(cc, "M.P. " + std::to_string(md.first) + ", " + std::to_string(md.second)); continue; }
        ct(cc, "Incorrect");
    }
}