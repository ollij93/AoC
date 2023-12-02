#include "day1/day1.hpp"
#include "day2/day2.hpp"

int main() {
    const Solution *solutions[] = {new Day1Solution(), new Day2Solution()};
    for (auto solution : solutions) {
        solution->run();
    }
    return 0;
}
