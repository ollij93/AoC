#include "day2.hpp"
#include <vector>

using namespace std;

struct BallCounts {
    int red = 0;
    int green = 0;
    int blue = 0;

    bool operator<=(const BallCounts &other) {
        return (red <= other.red && green <= other.green && blue <= other.blue);
    }

    int power() const { return red * green * blue; }
};

struct Game {
    int num;
    vector<BallCounts> counts;

    BallCounts maxes() const {
        auto ret = BallCounts();
        for (auto &counts : this->counts) {
            if (counts.red > ret.red) {
                ret.red = counts.red;
            }
            if (counts.green > ret.green) {
                ret.green = counts.green;
            }
            if (counts.blue > ret.blue) {
                ret.blue = counts.blue;
            }
        }
        return ret;
    }

    bool possible(const BallCounts &maxCounts) const {
        return this->maxes() <= maxCounts;
    }
};

vector<string> split(const string &s, const string &delim) {
    auto ret = vector<string>();
    size_t start = 0;
    size_t end;
    size_t delim_len = delim.length();

    while ((end = s.find(delim, start)) != std::string::npos) {
        ret.push_back(s.substr(start, end - start));
        start = end + delim_len;
    }
    ret.push_back(s.substr(start));
    return ret;
}

BallCounts parse_count(const string &s) {
    auto ret = BallCounts();
    auto parts = split(s, ", ");
    for (auto &part : parts) {
        int n = stoi(part);
        auto color = part.substr(to_string(n).length() + 1);
        if (color == "red") {
            ret.red = n;
        } else if (color == "green") {
            ret.green = n;
        } else if (color == "blue") {
            ret.blue = n;
        }
    }
    return ret;
}

Game parse_line(const string &line) {
    string numstr = line.substr(sizeof("Game ") - 1);
    int num = stoi(numstr);
    string countsstr = line.substr(line.find(":") + 2);
    auto counts = split(countsstr, "; ");
    auto bcs = vector<BallCounts>();
    for (auto &count : counts) {
        bcs.push_back(parse_count(count));
    }
    return {num, bcs};
}

vector<Game> parse_input(istream &in) {
    auto result = vector<Game>();
    for (string line; getline(in, line);) {
        result.push_back(parse_line(line));
    }
    return result;
}

int Day2Solution::part1(istream &in) const {
    auto games = parse_input(in);
    int sum = 0;
    for (auto &game : games) {
        if (game.possible({12, 13, 14})) {
            sum += game.num;
        }
    }
    return sum;
}
int Day2Solution::part2(istream &in) const {
    auto games = parse_input(in);
    int sum = 0;
    for (auto &game : games) {
        sum += game.maxes().power();
    }
    return sum;
}
