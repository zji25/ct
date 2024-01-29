#include "../include/top-down-parser.h"
#include <fstream>
#include <string>
#include <iostream>

grammar::grammar(const std::string&input) : input_file(input) {}
std::set<std::string> grammar::get_first_alpha(std::vector<std::string>&alpha) {
    if (alpha[0]==EPSILON) return {alpha[0]};
    for (auto&t:terminals) if (alpha[0]==t) return {t};
    assert(FIRST.find(alpha[0])!=FIRST.end());
    auto firstA=FIRST[alpha[0]];
    if (firstA.find(EPSILON) != firstA.end()) {
        if (alpha.size()>1) {
            firstA.erase(EPSILON);
            auto beta = alpha;
            beta.erase(beta.begin());
            auto firstBeta = get_first_alpha(beta);
            firstA.insert(firstBeta.begin(), firstBeta.end());
        }
    }
    return firstA;
}
auto cmp = [](const std::string& lhs, const std::string& rhs) { return lhs.length() > rhs.length(); };
void grammar::parse_grammar() {
    std::ifstream ifs;
    ifs.open(input_file);
    std::string line;
    if (ifs.is_open()) {
        std::getline(ifs, line); assert(line=="terminals:");
        while (std::getline(ifs, line)) { if (line.empty()) break; terminals.emplace_back(line); }
        std::sort(terminals.begin(), terminals.end(), cmp);
        std::getline(ifs, line); assert(line=="nonterminals:");
        while (std::getline(ifs, line)) { if (line.empty()) break; nonterminals.emplace_back(line); }
        std::sort(nonterminals.begin(), nonterminals.end(), cmp);
        std::getline(ifs, line); assert(line.substr(0, 3)=="S: ");
        S=line.substr(3,line.length()-3);
        std::getline(ifs, line);
        std::getline(ifs, line); assert(line=="rules:");
        while (std::getline(ifs, line)) {
            if (line.empty()) break;
            size_t i=line.find("->");
            std::string left = line.substr(0, i);
            std::vector<std::string> right;
            i+=2;
            while (true) {
                for (auto&t:terminals) if (line.substr(i, t.length()) == t) {
                    right.emplace_back(t); i+=t.length(); break;
                }
                if (i==line.length()) break;
                for (auto&nt:nonterminals) if (line.substr(i, nt.length()) == nt) {
                    right.emplace_back(nt); i+=nt.length(); break;
                }
                if (i==line.length()) break;
            }
            rules.emplace_back(left, right);
        }
    }
    for (auto&nt:nonterminals) FIRST[nt]={};
    bool changed=true;
    while (changed) {
        changed=false;
        for (auto&[A,alpha]:rules) {
            auto firstAlpha=get_first_alpha(alpha);
            for (auto&x:firstAlpha) {
                auto [_,inserted] = FIRST[A].insert(x);
                if (inserted) changed=true;
            }
        }
    }
    for (auto&nt:nonterminals) FOLLOW[nt]={};
    FOLLOW[S].emplace(token_to_string[END]);
    changed=true;
    while (changed) {
        changed=false;
        for (auto&[A,alpha]:rules) {
            for (auto B=alpha.begin();B!=alpha.end();++B) {
                if (FOLLOW.find(*B)!=FOLLOW.end()) {
                    std::vector<std::string> gamma(B+1,alpha.end());
                    if (gamma.empty()) gamma.emplace_back(token_to_string[END]);
                    auto firstGamma=get_first_alpha(gamma);
                    if (firstGamma.find(EPSILON) != firstGamma.end()) {
                        firstGamma.erase(EPSILON);
                        for (auto&x:FOLLOW[A]) {
                            auto [_,inserted] = FOLLOW[*B].insert(x);
                            if (inserted) changed=true;
                        }
                    }
                    for (auto&x:firstGamma) {
                        auto [_,inserted] = FOLLOW[*B].insert(x);
                        if (inserted) changed=true;
                    }
                }
            }

        }
    }
}
void grammar::print_grammar(const std::string&output_file) {
    size_t max_nt_length = (*std::max_element(nonterminals.begin(), nonterminals.end(),
                               [](const auto& a, const auto& b) { return a.size() < b.size(); })).length();
    std::ofstream ofs;
    ofs.open(output_file);
    if (ofs.is_open()) {
        ofs << std::string(max_nt_length+2, ' ') << "FIRST\n";
        for (auto&[nt,first]:FIRST) {
            ofs << nt << std::string(max_nt_length+2-nt.length(), ' ');
            for (auto&x:first) ofs << x << ' '; ofs << '\n';
        }
        ofs << '\n' << std::string(max_nt_length+2, ' ') << "FOLLOW\n";
        for (auto&[nt,follow]:FOLLOW) {
            ofs << nt << std::string(max_nt_length+2-nt.length(), ' ');
            for (auto&x:follow) ofs << x << ' '; ofs << '\n';
        }
    }
}
