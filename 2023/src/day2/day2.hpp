#include "common.hpp"

class Day2Solution : public Solution {
  public:
    std::string name() const final override { return "Day 2"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day2Solution()
        : Solution("2023/src/day2/day2.input", 8, 2286) {}
};
