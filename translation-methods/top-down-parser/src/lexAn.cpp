#include "../include/top-down-parser.h"
#include <string>
#include <sstream>

lexAn::lexAn(std::string& input) : input(input), pos(0) { next_token(); }
lexAn::lexAn() {}
void lexAn::next_token() {
    while (pos<input.length()&&std::isspace(input.at(pos))) ++pos;
    if (pos==input.length()) { c_token=END; return; }
    switch (input.at(pos)) {
        case '+': ++pos; c_token=PLUS; break;
        case '-': ++pos; c_token=MINUS; break;
        case '*': ++pos; c_token=MULTIPLY; break;
        case '/': ++pos; c_token=DIVIDE; break;
        case '(': ++pos; c_token=LPAREN; break;
        case ')': ++pos; c_token=RPAREN; break;
        case ',': ++pos; c_token=COMMA; break;
        default:
            int start=pos;
            while (pos<input.length()&&'0'<=input.at(pos)&&input.at(pos)<='9') ++pos;
            if (start!=pos) {
                c_token=NUMBER; c_value=input.substr(start, pos - start); break;
            }
            while (pos<input.length()&&std::isalpha(input.at(pos))) ++pos;
            if (start!=pos) {
                c_token=FUNCTION; c_value=input.substr(start, pos - start); break;
            }
            std::stringstream msg;
            msg << "\nwrong input format\n\tmarked [ <--- ] at index " << pos << "\n\t";
            msg << input.substr(0,pos+1) << "[ <--- ]" << input.substr(pos+1) << '\n';
            throw std::invalid_argument(msg.str());
    }
}
token lexAn::current_token() { return c_token; }
std::string lexAn::current_token_value() {
    if (c_token != NUMBER && c_token != FUNCTION) throw std::exception();
    return c_value;
}
