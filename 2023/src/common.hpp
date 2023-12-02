#pragma once
#include <fstream>
#include <iostream>

/**
 * Base class for all solutions.
 */
class Solution {
  public:
    virtual std::string name() const = 0;
    virtual int part1(std::istream &in) const = 0;
    virtual int part2(std::istream &in) const = 0;

    /**
     * Run both parts of the solution and print the results.
     */
    void run() const {
        std::ifstream input(inputfilepath);
        std::cout << name() << ", Part 1: " << part1(input) << std::endl;
        input.close();
        input.open(inputfilepath);
        std::cout << name() << ", Part 2: " << part2(input) << std::endl;
        input.close();
    }

    Solution(std::string inputfilepath,
             int exp_test_result_p1,
             int exp_test_result_p2)
        : inputfilepath(inputfilepath),
          exp_test_result_p1(exp_test_result_p1),
          exp_test_result_p2(exp_test_result_p2){};

    std::string inputfilepath;
    int exp_test_result_p1;
    int exp_test_result_p2;
};
