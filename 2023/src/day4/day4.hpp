#include <set>
#include <vector>

#include "common.hpp"

class Day4Solution : public Solution {
  public:
    std::string name() const final override { return "Day 4"; };
    int part1(std::istream &in) const final override;
    int part2(std::istream &in) const final override;

    Day4Solution()
        : Solution("2023/src/day4/day4.input", 13, 30) {}

  private:
    struct Card {
        std::set<int> winning_nums = std::set<int>();
        std::set<int> card_nums = std::set<int>();
    };

    std::vector<Card> parse_input(std::istream &in) const;
};
