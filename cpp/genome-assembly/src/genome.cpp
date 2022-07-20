#include "genome.h"

#include <algorithm>
#include <iterator>
#include <unordered_map>

namespace genome {

std::string assembly(const size_t k, const std::vector<std::string> & reads)
{
    if (k == 0 || reads.empty()) {
        return "";
    }
    const int n = reads.size();
    const int m = reads[0].size() - k;
    const size_t amount = m * n + 1;
    std::unordered_map<std::string, int> index;
    std::vector<int> counter(amount, 0);
    std::vector<std::unordered_map<int, int>> edges(amount);
    std::vector<std::string> back;
    std::string even, odd;
    std::pair<std::unordered_map<std::string, int>::iterator, bool> even_it, odd_it;
    for (int i = 0; i < n; i++) {
        even = reads[i].substr(0, k);
        even_it = index.try_emplace(even, index.size());
        if (even_it.second) {
            back.push_back(even);
        }
        counter[even_it.first->second]++;
        for (int j = 1; j < m + 1; j++) {
            if (j % 2 == 1) {
                odd = reads[i].substr(j, k);
                odd_it = index.try_emplace(odd, index.size());
                if (odd_it.second) {
                    back.push_back(odd);
                }
                edges[even_it.first->second][odd_it.first->second]++;
            }
            else {
                even = reads[i].substr(j, k);
                even_it = index.try_emplace(even, index.size());
                if (even_it.second) {
                    back.push_back(even);
                }
                edges[odd_it.first->second][even_it.first->second]++;
            }
        }
        if (m % 2) {
            counter[odd_it.first->second]--;
        }
        else {
            counter[even_it.first->second]--;
        }
    }

    int v = 0;
    for (size_t i = 0; i < counter.size(); i++) {
        if (counter[i] == 1) {
            v = i;
            break;
        }
    }
    std::vector<int> stack = {v};
    std::vector<int> result;
    bool found_edge;
    while (!stack.empty()) {
        if (stack.size() + result.size() == amount) {
            std::reverse(stack.begin(), stack.end());
            result.insert(result.end(), stack.begin(), stack.end());
            break;
        }
        v = stack.back();
        found_edge = false;
        for (auto u = edges[v].begin(); u != edges[v].end(); u++) {
            if (u->second > 0) {
                stack.push_back(u->first);
                u->second--;
                found_edge = true;
                break;
            }
        }
        if (!found_edge) {
            stack.pop_back();
            result.push_back(v);
        }
    }

    std::string genome = back[result[amount - 1]];
    genome.reserve(k + amount - 1);
    for (size_t i = 2; i < amount + 1; i++) {
        genome += back[result[amount - i]].at(k - 1);
    }
    return genome;
}

} // namespace genome