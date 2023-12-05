#include "common.hpp"

class Day5Solution : public Solution {
  public:
    std::string name() const final override { return "Day 5"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day5Solution()
        : Solution("2023/src/day5/day5.input", 35, 46) {}

};
