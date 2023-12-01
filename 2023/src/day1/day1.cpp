#include "day1.hpp"
#include <vector>

using namespace std;

const vector<const string> DIGIT_STRINGS = {
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

optional<tuple<int, int>> parse_line(string line, bool inc_strings = false) {
    auto digits = vector<int>();
    for (auto &c : line) {
        if (isdigit(c)) {
            digits.push_back(c - '0');
        } else if (inc_strings) {
            // For part 2, check if the character is the start of a digit
            for (int i = 0; i < DIGIT_STRINGS.size(); i++) {
                auto &s = DIGIT_STRINGS.at(i);
                if (strncmp(&c, s.c_str(), s.length()) == 0) {
                    digits.push_back(i + 1); // one-indexed
                }
            }
        }
    }
    if (digits.size() == 0) {
        return nullopt;
    }
    return tuple<int, int>(digits.front(), digits.back());
}

vector<tuple<int, int>> parse_input(istream &in, bool inc_strings = false) {
    auto result = vector<tuple<int, int>>();
    for (string line; getline(in, line);) {
        auto pair = parse_line(line, inc_strings);
        if (pair.has_value()) {
            result.push_back(pair.value());
        }
    }
    return result;
}

int Day1Solution::part1(istream &in) const {
    auto pairs = parse_input(in);
    auto sum = 0;
    for (auto &pair : pairs) {
        auto value = 10 * get<0>(pair) + get<1>(pair);
        sum += value;
    }
    return sum;
}

int Day1Solution::part2(istream &in) const {
    auto pairs = parse_input(in, true);
    auto sum = 0;
    for (auto &pair : pairs) {
        auto value = 10 * get<0>(pair) + get<1>(pair);
        sum += value;
    }
    return sum;
}
