#define CATCH_CONFIG_MAIN
#include <catch2/catch_test_macros.hpp>

#include "day1/day1.hpp"
#include "day2/day2.hpp"
#include "day3/day3.hpp"
#include "day4/day4.hpp"

TEST_CASE("Day1Solution", "[day1]") {
    auto solution = Day1Solution();
    SECTION("Part 1") {
        std::ifstream input("2023/src/day1/day1.testinput1");
        int result = solution.part1(input);
        int expected = solution.exp_test_result_p1;
        REQUIRE(result == expected);
        input.close();
    }
    SECTION("Part 2") {
        std::ifstream input("2023/src/day1/day1.testinput2");
        int result = solution.part2(input);
        int expected = solution.exp_test_result_p2;
        REQUIRE(result == expected);
        input.close();
    }
}

TEST_CASE("Day2Solution", "[day2]") {
    auto solution = Day2Solution();
    std::ifstream input("2023/src/day2/day2.testinput");
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

TEST_CASE("Day3Solution", "[day3]") {
    auto solution = Day3Solution();
    std::ifstream input("2023/src/day3/day3.testinput");
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

TEST_CASE("Day4Solution", "[day4]") {
    auto solution = Day4Solution();
    std::ifstream input("2023/src/day4/day4.testinput");
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
