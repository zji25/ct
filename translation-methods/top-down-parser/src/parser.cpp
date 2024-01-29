#include "../include/top-down-parser.h"
#include <iostream>

parser::parser(const std::string&grammar_input_file) : gram(grammar(grammar_input_file)) {
    gram.parse_grammar();
}

tree* parser::get_tree(std::string&line) {
    lex=lexAn(line);
    return E();
}

tree* parser::E() { /*  E -> TE'  first(TE') = first[T] = {(,-,f,n}  */
    tree*r=new tree("E");
    switch (lex.current_token()) {
        case NUMBER:
        case MINUS:
        case LPAREN:
        case FUNCTION: {
            r->children.emplace_back(T());
            r->children.emplace_back(Ei());
            break;
        }
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}
tree* parser::F() {
    /*
     F -> n    first(n)    = {n}
     F -> -F   first(-F)   = {-}
     F -> (E)  first((E))  = {(}
     F -> f(A) first(f(A)) = {f}
    */
    tree*r=new tree("F");
    switch (lex.current_token()) {
        case NUMBER: {
            r->children.emplace_back(new tree(lex.current_token_value()));
            lex.next_token();
            break;
        }
        case MINUS: {
            lex.next_token();
            r->children = {new tree(token_to_string[MINUS]), F()};
            break;
        }
        case LPAREN: {
            lex.next_token();
            tree*e=E();
            assert(lex.current_token() == RPAREN);
            lex.next_token();
            r->children = {new tree(token_to_string[LPAREN]), e, new tree(token_to_string[RPAREN])};
            break;
        }
        case FUNCTION: {
            tree*fn = new tree(lex.current_token_value());
            lex.next_token();
            assert(lex.current_token() == LPAREN);
            lex.next_token();
            tree*a=A();
            assert(lex.current_token() == RPAREN);
            lex.next_token();
            r->children = {fn, new tree(token_to_string[LPAREN]), a, new tree(token_to_string[RPAREN])};
            break;
        }
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}

tree* parser::T() { /*  T -> FT'  first(FT') = first[F] = {(,-,f,n}  */
    tree*r=new tree("T");
    switch (lex.current_token()) {
        case NUMBER:
        case MINUS:
        case LPAREN:
        case FUNCTION:
            r->children.emplace_back(F());
            r->children.emplace_back(Ti());
            break;
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}

tree* parser::A() { /*  A -> EA'  first(EA') = first[E] = {(,-,f,n} */
    tree*r=new tree("A");
    switch (lex.current_token()) {
        case NUMBER:
        case MINUS:
        case LPAREN:
        case FUNCTION:
            r->children.emplace_back(E());
            r->children.emplace_back(Ai());
            break;
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}

tree *parser::Ei() {
    /*
     E' -> +TE' first(+TE') = {+}
     E' -> -TE' first(-TE') = {-}
     E' -> e    first(e)    = {e}
    */
    tree*r=new tree("E'");
    switch (lex.current_token()) {
        case PLUS:
        case MINUS:
            r->children.emplace_back(new tree(token_to_string[lex.current_token()]));
            lex.next_token();
            r->children.emplace_back(T());
            r->children.emplace_back(Ei());
            break;
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}

tree *parser::Ti() {
    /*
     T' -> *FT'  first(*FT') = {*}
     T' -> /FT'  first(/FT') = {/}
     T' -> e     first(e)    = {e}
    */
    tree*r=new tree("T'");
    switch (lex.current_token()) {
        case MULTIPLY:
        case DIVIDE:
            r->children.emplace_back(new tree(token_to_string[lex.current_token()]));
            lex.next_token();
            r->children.emplace_back(F());
            r->children.emplace_back(Ti());
            break;
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}

tree *parser::Ai() {
    /*
     A' -> ,EA'  first(,EA') = {,}
     A' -> e     first(e)    = {e}
    */
    tree*r=new tree("A'");
    switch (lex.current_token()) {
        case COMMA:
            r->children.emplace_back(new tree(token_to_string[lex.current_token()]));
            lex.next_token();
            r->children.emplace_back(E());
            r->children.emplace_back(Ai());
            break;
        default:
            r->children.emplace_back(new tree(EPSILON));
            break;
    }
    return r;
}

grammar parser::get_grammar() {
    return gram;
}
