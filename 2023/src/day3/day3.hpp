#include "common.hpp"

class Day3Solution : public Solution {
  public:
    std::string name() const final override { return "Day 3"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day3Solution()
        : Solution("2023/src/day3/day3.input", 4361, 0) {}
};
