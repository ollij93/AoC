#include <vector>
#include "day1.hpp"

int Day1Solution::part1(std::istream &in) const {
    auto calories = std::vector<int>();
    calories.push_back(0);

    for (std::string line; std::getline(in, line);) {
        try {
            int num = std::stoi(line);
            calories.back() += num;
        } catch (const std::invalid_argument &ia) {
            /* Anything that isn't a valid integer line is treated as a break */
            calories.push_back(0);
        }
    }
    return *std::max_element(calories.begin(), calories.end());
}

int Day1Solution::part2(std::istream &in) const {
    auto calories = std::vector<int>();
    calories.push_back(0);

    for (std::string line; std::getline(in, line);) {
        try {
            int num = std::stoi(line);
            calories.back() += num;
        } catch (const std::invalid_argument &ia) {
            /* Anything that isn't a valid integer line is treated as a break */
            calories.push_back(0);
        }
    }
    std::sort(calories.begin(), calories.end(), std::greater<int>());
    return calories.at(0) + calories.at(1) + calories.at(2);
}
