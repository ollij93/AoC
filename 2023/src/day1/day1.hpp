#include "common.hpp"

class Day1Solution : public Solution {
  public:
    std::string name() const final override { return "Day 1"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day1Solution()
        : Solution("2023/src/day1/day1.input", 142, 281) {}
};
