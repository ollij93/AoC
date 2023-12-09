#include "day6/day6.hpp"

#include "common.hpp"

#include <algorithm>

using namespace std;

class Race {
  public:
    Race(long time_limit, long record)
        : time_limit(time_limit),
          record(record) {}

    long calc_dist(long hold_time) const {
        if (hold_time > time_limit) {
            return 0;
        }
        return (time_limit - hold_time) * hold_time;
    }

    long find_lower_hold_limit() const {
        // Can't move at all holding for < 1 time
        for (long time = 1; time < time_limit; time++) {
            if (calc_dist(time) > record) {
                return time;
            }
        }
        return 0;
    }

    long find_upper_hold_limit() const {
        // Can't move at all holding for >= time_limit time
        for (long time = time_limit - 1; time > 0; time--) {
            if (calc_dist(time) > record) {
                return time;
            }
        }
        return 0;
    }

    long find_hold_limit_range() const {
        return find_upper_hold_limit() - find_lower_hold_limit() + 1;
    }

  private:
    long time_limit;
    long record;
};

vector<long> parse_nums(string line) {
    vector<long> ret;
    auto parts = split(line, " ");
    for (auto part : parts) {
        if (part == "") {
            continue;
        }
        try {
            ret.push_back(stoi(part));
        } catch (invalid_argument) {
            continue;
        }
    }
    return ret;
}

vector<Race> parse_input_p1(istream &in) {
    string timeline;
    getline(in, timeline);
    string distline;
    getline(in, distline);

    auto times = parse_nums(timeline);
    auto records = parse_nums(distline);

    auto ret = vector<Race>();
    for (long i = 0; i < times.size(); i++) {
        ret.push_back(Race(times[i], records[i]));
    }
    return ret;
}
vector<Race> parse_input_p2(istream &in) {
    string timeline;
    getline(in, timeline);
    string distline;
    getline(in, distline);

    auto timesubstr = timeline.substr(timeline.find(":") + 1);
    auto distsubstr = distline.substr(distline.find(":") + 1);
    timesubstr.erase(remove(timesubstr.begin(), timesubstr.end(), ' '),
                     timesubstr.end());
    distsubstr.erase(remove(distsubstr.begin(), distsubstr.end(), ' '),
                     distsubstr.end());

    auto ret = vector<Race>();
    ret.push_back(Race(stol(timesubstr), stol(distsubstr)));
    return ret;
}

int Day6Solution::part1(std::istream &in) const {
    auto races = parse_input_p1(in);
    long prod = 1;
    for (auto race : races) {
        prod *= race.find_hold_limit_range();
    }
    return prod;
}

int Day6Solution::part2(std::istream &in) const {
    auto races = parse_input_p2(in);
    long prod = 1;
    for (auto race : races) {
        prod *= race.find_hold_limit_range();
    }
    return prod;
}
