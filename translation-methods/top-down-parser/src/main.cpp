#include "../include/top-down-parser.h"
#include <fstream>

int main() {
    system("rm -f -r ../generated-files/*\nmkdir -p ../generated-files");
    parser pars = parser("../input-files/grammar.txt");
    pars.get_grammar().print_grammar("../generated-files/first&follow.txt");
    std::ifstream ifs;
    ifs.open("../input-files/tests.txt");
    std::string line;
    if (ifs.is_open()) {
        while (std::getline(ifs, line)) {
            tree*tr = pars.get_tree(line);
            tr->to_dot_file("../generated-files/graph.dot");
            system("a=$(find ../generated-files -name \"*png\" | wc -l)\n"
                   "dot -Tpng ../generated-files/graph.dot -o ../generated-files//test$(($a + 1)).png");
            system("rm -f ../generated-files/graph.dot");
        }
    }
}
