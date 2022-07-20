#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <random>
#include <string>
#include <vector>

#define win 0
#define file_error 1
#define invalid_data 3

const int LIMIT = 10000;

template <typename T>
void shuffle_vector(std::vector<T> & vector,
                    const bool is_repeat,
                    const bool is_count,
                    const int count)
{
    int limit = is_count ? count : LIMIT;
    if (is_repeat) {
        for (int k = 0; k < limit; k++) {
            std::cout << vector[rand() % vector.size()] << '\n';
        }
    }
    else {
        std::random_device galaxy_A32_5G;
        std::mt19937 g(galaxy_A32_5G());
        std::shuffle(vector.begin(), vector.end(), g);
        int f = std::min(limit, static_cast<int>(vector.size()));
        for (int k = 0; k < f; k++) {
            std::cout << vector[k] << '\n';
        }
    }
}
enum string_code
{
    head_count,
    input_range,
    repeat,
    unknown
};
const std::map<string_code, std::string> code_to_string_map = {
        {head_count, "--head-count="},
        {input_range, "--input-range="},
        {repeat, "--repeat"}};

string_code hash(const std::string & argument)
{
    if (argument == "-n" || argument.rfind(code_to_string_map.at(head_count), 0) == 0) {
        return head_count;
    }
    if (argument == "-i" || argument.rfind(code_to_string_map.at(input_range), 0) == 0) {
        return input_range;
    }
    if (argument == "-r" || argument == code_to_string_map.at(repeat)) {
        return repeat;
    }
    return unknown;
}

int main(const int argc, const char * argv[])
{
    const int range_offset = code_to_string_map.at(input_range).length();
    const int count_offset = code_to_string_map.at(head_count).length();
    bool is_range = false;
    bool is_count = false;
    bool is_repeat = false;
    std::pair<unsigned int, unsigned int> low_high;
    int count;
    int index;
    int offset;
    bool is_short;
    string_code current_code;
    for (int j = 1; j < argc; j++) {
        is_short = false;
        current_code = hash(argv[j]);
        if (strlen(argv[j]) == 2 && current_code != unknown) {
            j++;
            if (j == argc) {
                std::cerr << "missing argument\n";
                return invalid_data;
            }
            is_short = true;
        }
        switch (current_code) {
        case repeat:
            is_repeat = true;
            break;
        case head_count:
            is_count = true;
            if (is_short) {
                count = std::stoi(argv[j]);
            }
            else {
                count = std::stoi(std::string(argv[j] + count_offset, strlen(argv[j]) - count_offset));
            }
            break;
        case input_range:
            is_range = true;
            offset = is_short ? 0 : range_offset;
            index = std::find(argv[j] + offset, argv[j] + strlen(argv[j]), '-') - argv[j];
            low_high = {std::stoi(std::string(argv[j] + offset, index)),
                        std::stoi(std::string(argv[j] + index + 1, strlen(argv[j]) - index))};
            break;
        case unknown:
            if (j != argc - 1) {
                std::cerr << "unknown argument\n";
                return invalid_data;
            }
            break;
        }
    }
    if (is_range) {
        std::vector<unsigned int> numbers;
        numbers.resize(low_high.second - low_high.first + 1);
        for (unsigned int k = low_high.first; k <= low_high.second; ++k) {
            numbers[k - low_high.first] = k;
        }
        shuffle_vector(numbers, is_repeat, is_count, count);
    }
    else {
        std::vector<std::string> lines;
        std::ifstream file(argv[argc - 1]);
        std::string current;
        if (file.is_open()) {
            while (!file.eof()) {
                getline(file, current);
                lines.push_back(current);
            }
        }
        else {
            std::cerr << "can't open file\n";
            return file_error;
        }
        if (!lines.empty() && lines.back().empty()) {
            lines.pop_back();
        }
        if (!lines.empty()) {
            shuffle_vector(lines, is_repeat, is_count, count);
        }
    }
    return win;
}