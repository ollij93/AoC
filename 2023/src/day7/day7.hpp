#include "common.hpp"

class Day7Solution : public Solution {
  public:
    std::string name() const final override { return "Day 7"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day7Solution()
        : Solution("2023/src/day7/day7.input", 6440, 5905) {}

};
