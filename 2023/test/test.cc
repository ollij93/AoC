#define CATCH_CONFIG_MAIN
#include <catch2/catch_test_macros.hpp>

#include "day1/day1.h"

TEST_CASE("Day1Solution", "[day1]") {
  auto solution = Day1Solution();
  std::ifstream input(solution.testfilepath);
  SECTION("Part 1") {
    int result = solution.part1(input);
    int expected = solution.exp_test_result_p1;
    REQUIRE(result == expected);
  }
  SECTION("Part 2") {
    int result = solution.part2(input);
    int expected = solution.exp_test_result_p2;
    REQUIRE(result == expected);
  }
  input.close();
}
