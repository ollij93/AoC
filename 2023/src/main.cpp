#include "day1/day1.hpp"

int main() {
    const Solution *solutions[] = {new Day1Solution()};
    for (auto solution : solutions) {
        solution->run();
    }
    return 0;
}
