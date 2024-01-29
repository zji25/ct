#include "../include/top-down-parser.h"
#include <utility>
#include <iostream>
#include <fstream>
#include <unordered_map>

tree::tree(std::string nd) : node(std::move(nd)) {}

void add_edges(tree* root, std::vector<std::string>& labels, std::vector<std::string>& edges) {
    root->set_index(labels.size());
    labels.emplace_back(root->node);
    for (auto*child:root->children) {
        add_edges(child,labels,edges);
        edges.emplace_back(std::to_string(root->get_index())+" -> "+std::to_string(child->get_index()));
    }
}
void tree::to_dot_file(const std::string& output_file) {
    std::vector<std::string> labels, edges;
    add_edges(this, labels, edges);
    std::ofstream ofs;
    ofs.open(output_file);
    if (ofs.is_open()) {
        ofs << "digraph G {\n";
        for (int i=0;i<labels.size();++i) ofs << i << " [label=\"" << labels[i] << "\"];\n";
        for (auto&edge:edges) ofs << edge << ";\n";
        ofs << "}\n";
    }
}

void tree::set_index(int i) {
    index = i;
}

int tree::get_index() {
    return index;
}

