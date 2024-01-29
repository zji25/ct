#ifndef TOPDOWN
#include <unordered_map>
#include <string>
#include <vector>
#include <unordered_set>
#include <set>
#include <map>
#include <iostream>
#include <cassert>

enum token {
    NUMBER, FUNCTION, PLUS, MINUS, MULTIPLY, DIVIDE, LPAREN, RPAREN, COMMA, END
};
static std::unordered_map<token,std::string> token_to_string {
        {PLUS, "+"}, {MINUS, "-"}, {MULTIPLY, "*"}, {DIVIDE, "/"}, {LPAREN, "("}, {RPAREN, ")"}, {COMMA, ","}, {END, "$"}
};

const std::string EPSILON = "\u03B5";

class lexAn {
    std::string input, c_value; int pos; token c_token;
public:
    lexAn(std::string& input);
    lexAn();
    void next_token();
    token current_token();
    std::string current_token_value();
};

class grammar {
    std::string input_file;
    std::set<std::string> get_first_alpha(std::vector<std::string>&alpha);
public:
    std::string S;
    std::vector<std::string> nonterminals, terminals;
    std::vector<std::pair<std::string,std::vector<std::string>>> rules;
    std::map<std::string,std::set<std::string>> FIRST, FOLLOW;
    explicit grammar(const std::string& input_file);
    void parse_grammar();
    void print_grammar(const std::string& output_file);
};

class tree {
    int index;
    std::vector<std::string> lines;
    std::vector<std::string> colors;
public:
    std::string node;
    std::vector<tree*> children;
    tree(std::string node);
    void to_dot_file(const std::string& output_file);
    void set_index(int index);
    int get_index();
};

class parser {
    lexAn lex;
    grammar gram;
    tree*E();
    tree*F();
    tree*T();
    tree*Ei();
    tree*Ti();
    tree*A();
    tree*Ai();
public:
    explicit parser(const std::string&grammar_input_file);
    grammar get_grammar();
    tree* get_tree(std::string& line);
};
#endif
#define TOPDOWN