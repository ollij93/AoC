#include "common.hpp"

class Day6Solution : public Solution {
  public:
    std::string name() const final override { return "Day 6"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day6Solution()
        : Solution("2023/src/day6/day6.input", 288, 71503) {}

};
