#include <iostream>
#include <unordered_map>
#include <vector>
#include <algorithm>

struct expr {
    virtual bool equals(expr*e,bool sym)=0;
    virtual std::string to_str()=0;
    virtual ~expr()=default;
};
std::unordered_map<std::string, expr*> vars;
struct var : public expr {
    std::string _name;
    explicit var(std::string&name) : _name(name) {}
    bool equals(expr*e,bool sym) override {
        auto*ev = dynamic_cast<var*>(e);
        if (sym) return ev!=nullptr&&ev->_name==_name;
        if (vars.find(_name)!=vars.end()) {
            auto*fv=dynamic_cast<var*>(vars[_name]);
            if (fv!=nullptr) return ev!=nullptr&&ev->_name==fv->_name;
            return vars[_name]->equals(e,true);
        }
        vars[_name]=e;return true;
    }
    std::string to_str() override {return _name;}
};
struct impl : public expr {
    expr*_left; expr*_right;
    impl(expr*left,expr*right) : _left(left), _right(right) {}
    bool equals(expr*e,bool sym) override {
        auto*i=dynamic_cast<impl*>(e);
        if (i==nullptr) return false;
        return _left->equals(i->_left,sym)&&_right->equals(i->_right,sym);
    }
    std::string to_str() override { return "("+_left->to_str()+"->"+_right->to_str()+")"; }
};
struct disj : public expr {
    expr*_left; expr*_right;
    disj(expr*left,expr*right) : _left(left), _right(right) {}
    bool equals(expr*e,bool sym) override {
        auto*d=dynamic_cast<disj*>(e);
        if (d==nullptr) return false;
        return _left->equals(d->_left,sym)&&_right->equals(d->_right,sym);
    }
    std::string to_str() override { return "("+_left->to_str()+"|"+_right->to_str()+")"; }
};
struct conj : public expr {
    expr*_left; expr*_right;
    conj(expr*left, expr*right) : _left(left), _right(right) {}
    bool equals(expr*e,bool sym) override {
        auto*c=dynamic_cast<conj*>(e);
        if (c==nullptr) return false;
        return _left->equals(c->_left,sym) && _right->equals(c->_right,sym);
    }
    std::string to_str() override { return "("+_left->to_str()+"&"+_right->to_str()+")"; }
};
struct neg : public expr {
    expr*_expr;
    explicit neg(expr*expr) : _expr(expr) {}
    bool equals(expr*e,bool sym) override {
        auto*n=dynamic_cast<neg*>(e);
        if (n==nullptr) return false;
        return _expr->equals(n->_expr,sym);
    }
    std::string to_str() override { return "(!"+_expr->to_str()+")"; }
};

std::string input;
size_t inp_length, pos;
bool skip(const std::string& s) {
    while (pos<inp_length && isspace(input.at(pos))) ++pos;
    size_t i=pos;
    for (size_t j=0;j<s.length();++i,++j) if (inp_length<=i||input.at(i)!=s.at(j)) return false;
    pos=i;
    return true;
}
expr* p_expr();
expr* p_var() {
    std::string x;
    while (pos<inp_length && isspace(input.at(pos))) ++pos;
    while (pos<inp_length && (int(input.at(pos))==39
        ||('0'<=input.at(pos)&&input.at(pos)<='9')||('A'<=input.at(pos)&&input.at(pos)<='Z'))) {
        x+=input.at(pos++);
    }
    if (x.empty()) return nullptr;
    return new var(x);
}
expr* p_neg() {
    if (skip("(")) { auto*x=p_expr(); skip(")"); return x; }
    if (skip("!")) return new neg(p_neg());
    return p_var();
}
expr* p_con() {
    auto*x=p_neg();
    if (x==nullptr) return x;
    while (skip("&")) x=new conj(x,p_neg());
    return x;
}
expr* p_dis() {
    auto*x=p_con();
    if (x==nullptr) return x;
    while (skip("|")) {
        if (input.at(pos)=='-') {--pos; break;}
        else x=new disj(x,p_con());
    }
    return x;
}
expr* p_expr() {
    auto*x=p_dis();
    if (x==nullptr) return x;
    if (skip("->")) return new impl(x,p_expr());
    return x;
}
std::vector<expr*> p_cntxt() {
    std::vector<expr*> cntxt;
    while (true) {
        if (skip("|-")) break;
        auto*e=p_expr();
        if (e!=nullptr) cntxt.push_back(e);
        if (skip(",")) continue;
    }
    return cntxt;
}

std::string sa = "a", sb = "b", sc = "c";
var *A = new var(sa), *B = new var(sb), *C = new var(sc);
std::vector<expr*> axioms {
        new impl(A, new impl(B, A)),
        new impl(new impl(A, B), new impl(new impl(A, new impl(B, C)), new impl(A, C))),
        new impl(A, new impl(B, new conj(A, B))),
        new impl(new conj(A, B), A), new impl(new conj(A, B), B),
        new impl(A, new disj(A, B)), new impl(B, new disj(A, B)),
        new impl(new impl(A, C), new impl(new impl(B, C), new impl(new disj(A, B), C))),
        new impl(new impl(A, B), new impl(new impl(A, new neg(B)), new neg(A))),
        new impl(new neg(new neg(A)), A)
};
int axiom(expr*expr) {
    for (int i=0;i<axioms.size();++i) {
        bool f=axioms[i]->equals(expr, false);
        vars.clear();
        if (f) return i+1;
    }
    return 0;
}
enum rule {AX, MP, HYP};
std::vector<std::pair<std::vector<expr*>, expr*>> inp;
void set() {inp_length=input.length(); pos=0;}
bool cc(std::vector<expr*>&sm, std::vector<expr*>&bg) {
    if (sm.size()>bg.size()) return false;
    std::vector<bool> dead(bg.size(),false);
    for (auto&sh:sm) {
        bool f=false;
        for (size_t j=0;j<bg.size();++j) if (!dead[j]) {
            if (sh->equals(bg[j],true)) {dead[j]=true; f=true; break;}
        }
        if (!f) return false;
    }
    return true;
}
std::vector<std::pair<expr*, rule>> mpp, mpp_sy;
std::pair<size_t,size_t> mp(size_t i) {
    for (size_t j=i;j--;) {
        auto*ab=dynamic_cast<impl*>(inp[j].second);
        if (ab==nullptr) continue;
        if (!ab->_right->equals(inp[i].second,true)) continue;
        if (!cc(inp[j].first,inp[i].first)) continue;
        for (size_t k=i;k--;) if (k!=j) {
            if (ab->_left->equals(inp[k].second,true)) {
                if (cc(inp[k].first,inp[i].first)) return {j+1,k+1};
            }
        }
    }
    return {0,0};
}
std::pair<size_t,std::vector<std::pair<bool,expr*>>> dd(size_t i) {
    std::vector<expr*>G(inp[i].first);
    auto*e=inp[i].second;
    while (true) {
        auto*ab=dynamic_cast<impl*>(e);
        if (ab==nullptr) break;
        G.emplace_back(ab->_left);
        e=ab->_right;
    }
    size_t d=0;
    for (size_t j=i;j--;) {
        std::vector<expr*>Gj(inp[j].first);
        auto*ej=inp[j].second;
        while (true) {
            auto*abj=dynamic_cast<impl*>(ej);
            if (abj==nullptr) break;
            Gj.emplace_back(abj->_left);
            ej=abj->_right;
        }
        if (e->equals(ej,true)) {if (cc(Gj,G)) {d=j+1;break;}}
    }
    if (d==0) return {0,{}};
    std::vector<std::pair<bool,expr*>> todo;
    auto*ed=inp[d-1].second;
    while (true) {
        e=inp[i].second;
        std::vector<expr*> mayb;
        while (true) {
            if (e->equals(ed,true)) {
                std::reverse(mayb.begin(),mayb.end());
                for (auto*v:mayb) todo.emplace_back(true,v);
                return {d,todo};
            }
            auto*abe=dynamic_cast<impl*>(e);
            if (abe==nullptr) break;
            mayb.emplace_back(abe->_left);
            e=abe->_right;
        }
        auto*abd=dynamic_cast<impl*>(ed);
        if (abd==nullptr) break;
        todo.emplace_back(false,abd->_left);
        ed=abd->_right;
    }
    return {0,{}};
}
void prove(size_t i) {
    auto*e=inp[i].second;
    for (auto*h:inp[i].first) {if (h->equals(e, true)) {mpp.emplace_back(e,HYP);return;}}
    if (axiom(e)>0) {mpp.emplace_back(e,AX);return;}
    auto m=mp(i);
    if (m.first!=0) {
        mpp.emplace_back(e,MP);
        prove(m.first-1);
        prove(m.second-1);
        return;
    }
    auto d=dd(i);
    if (d.first==0) return;
    size_t cs=mpp.size();
    prove(d.first-1);
    auto*ce=inp[d.first-1].second;
    for (auto&td:d.second) {
        std::vector<std::pair<expr*,rule>> temp;
        if (!td.first) {
            auto*imp=dynamic_cast<impl*>(ce);
            ce=imp->_right;
            for (size_t j=cs;j<mpp.size();++j) temp.emplace_back(mpp[j]);
            mpp.resize(cs);
            mpp.emplace_back(ce,MP);
            mpp.emplace_back(td.second,HYP);
            for (auto&v:temp) mpp.emplace_back(v);
            continue;
        }
        ce=new impl(td.second,ce);
        size_t k=mpp.size();
        while (true) {
            if (k==0) break;
            if (cs>0&&k==cs-1) break;
            --k;
            expr*current=mpp[k].first;
            rule cr=mpp[k].second;
            if (current->equals(td.second, true)) {
                auto*two=new impl(td.second,td.second);
                auto*three=new impl(td.second,two);
                auto*four=new impl(td.second,new impl(two,td.second));
                auto*six=new impl(four,two);
                temp.emplace_back(three,AX);
                temp.emplace_back(new impl(three,six),AX);
                temp.emplace_back(six,MP);
                temp.emplace_back(four,AX);
                temp.emplace_back(two,MP);
            } else if (cr==AX||cr==HYP) {
                auto* two = new impl(td.second,current);
                temp.emplace_back(new impl(current,two),AX);
                temp.emplace_back(current,HYP);
                temp.emplace_back(two,MP);
            } else {
                size_t sm=0, bg=0;
                for (size_t kk=k+1;kk<mpp.size();++kk) {
                    if (sm>0) break;
                    auto*ab=dynamic_cast<impl*>(mpp[kk].first);if (ab==nullptr) continue;
                    if (!ab->_right->equals(mpp[k].first,true)) continue;
                    for (size_t jj=k+1;jj<mpp.size();++jj) if (kk!=jj) if (ab->_left->equals(mpp[jj].first, true)) {
                        sm=jj+1;bg=kk+1;break;
                    }
                }
                auto*an=new impl(td.second, current);
                auto*ajnn=new impl(new impl(td.second, mpp[bg-1].first), an);
                temp.emplace_back(new impl(new impl(td.second, mpp[sm-1].first), ajnn),AX);
                temp.emplace_back(ajnn,MP);
                temp.emplace_back(an,MP);
            }
        }
        mpp.resize(cs);
        std::reverse(temp.begin(),temp.end());
        for (auto&v:temp) mpp.emplace_back(v);
    }
}
void simplify(size_t i) {
    auto*e=mpp[i].first;
    for (size_t j=i+1;j<mpp.size();++j) if (mpp[j].first->equals(e,true)) {simplify(j);return;}
    for (auto*h:inp[inp.size()-1].first) if (e->equals(h,true)) {mpp_sy.emplace_back(h,HYP);return;}
    if (axiom(e)>0) {mpp_sy.emplace_back(e,AX);return;}
    for (size_t k=i+1;k<mpp.size();++k) {
        auto*ab=dynamic_cast<impl*>(mpp[k].first);if (ab==nullptr) continue;
        if (!ab->_right->equals(e, true)) continue;
        for (size_t j=i+1;j<mpp.size();++j) if (k!=j) {
            if (ab->_left->equals(mpp[j].first,true)) {mpp_sy.emplace_back(e,MP);simplify(j);simplify(k);return;}
        }
    }
}
int main() {
    while (std::getline(std::cin,input)) {
        if (input.length()<=2) break;
        set();auto cntxt=p_cntxt();auto*expr=p_expr();inp.emplace_back(cntxt, expr);
    }
    if (inp.empty()) return 0;
    auto&last=inp[inp.size()-1];
    bool f=true;
    for (auto*v:last.first) {
        if (f) f=false,std::cout<<v->to_str();
        else std::cout<<","<<v->to_str();
    }
    std::cout<<"|-"<<last.second->to_str()<<'\n';
    prove(inp.size()-1);
    if (!mpp.empty()) {
        simplify(0);
        std::reverse(mpp_sy.begin(),mpp_sy.end());
        for (auto&v:mpp_sy) std::cout<<v.first->to_str()<<'\n';
    }
}