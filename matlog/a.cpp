#include <iostream>

std::string input;
int inp_length, pos;
bool skip(const std::string& s) {
    while (pos<inp_length && isspace(input.at(pos))) ++pos;
    int i = pos;
    for (int j=0; j<s.length(); ++i,++j) if (inp_length<=i || input.at(i)!=s.at(j)) return false;
    pos=i;
    return true;
}
std::string expression();
std::string variable() {
    std::string x;
    while (pos<inp_length && isspace(input.at(pos))) ++pos;
    while (pos<inp_length && (int(input.at(pos))==39
        || '0'<=input.at(pos) && input.at(pos)<='9' || 'A'<=input.at(pos) && input.at(pos)<='Z')) {
        x+=input.at(pos++);
    }
    return x;
}
std::string negation() {
    if (skip("(")) { std::string x = expression(); skip(")"); return x; }
    if (skip("!")) return "(!"+negation()+")";
    return variable();
}
std::string conjunction() {
    std::string x = negation();
    while (skip("&")) x = "(&,"+x+","+negation()+")";
    return x;
}
std::string disjunction() {
    std::string x = conjunction();
    while (skip("|")) x = "(|,"+x+","+conjunction()+")";
    return x;
}
std::string expression() {
    std::string x = disjunction();
    if (skip("->")) return "(->,"+x+","+expression()+")";
    return x;
}
int main() {
    std::cin >> input;
    inp_length = input.length(), pos = 0;
    std::cout << expression();
}