#include "day3/day3.hpp"

#include <map>

using namespace std;

bool is_symbol(char c) { return !((c >= '0' && c <= '9') || c == '.'); }

struct GearInfo {
    int r1;
    int r2;
};

int evaluate_line(int linenum,
                  string prevline,
                  string line,
                  string nextline,
                  map<tuple<int, int>, GearInfo> &gearratios) {
    int ret = 0;
    for (auto i = 0; i < line.length(); i++) {
        int n;
        try {
            n = stoi(line.substr(i));
        } catch (invalid_argument) {
            continue;
        }
        auto n_len = to_string(n).length();
        auto sublen = n_len + 2;
        if (i == 0) {
            sublen--;
        }
        auto istart = max(0, i - 1);
        auto prevsub = prevline.substr(istart, sublen);
        auto linesub = line.substr(istart, sublen);
        auto nextsub = nextline.substr(istart, sublen);
        bool after_sym = is_symbol(linesub.front());
        bool before_sym = is_symbol(linesub.back());
        bool below_sym = any_of(prevsub.begin(), prevsub.end(), is_symbol);
        bool above_sym = any_of(nextsub.begin(), nextsub.end(), is_symbol);
        if (after_sym || before_sym || below_sym || above_sym) {
            // Count the value
            ret += n;

            // Check for gears
            auto prevgear = prevsub.find('*');
            if (prevgear != string::npos) {
                auto prevgearpos = make_tuple(linenum - 1, prevgear + istart);
                if (gearratios.find(prevgearpos) != gearratios.end()) {
                    gearratios[prevgearpos].r2 = n;
                } else {
                    gearratios[prevgearpos] = {n, 0};
                }
            }
            auto linegear = linesub.find('*');
            if (linegear != string::npos) {
                auto linegearpos = make_tuple(linenum, linegear + istart);
                if (gearratios.find(linegearpos) != gearratios.end()) {
                    gearratios[linegearpos].r2 = n;
                } else {
                    gearratios[linegearpos] = {n, 0};
                }
            }
            auto nextgear = nextsub.find('*');
            if (nextgear != string::npos) {
                auto nextgearpos = make_tuple(linenum + 1, nextgear + istart);
                if (gearratios.find(nextgearpos) != gearratios.end()) {
                    gearratios[nextgearpos].r2 = n;
                } else {
                    gearratios[nextgearpos] = {n, 0};
                }
            }

            // Jump forward to the end of the number
            i += n_len - 1;
        }
    }
    return ret;
}

struct Results {
    int sum;
    map<tuple<int, int>, GearInfo> gearratios;
};

Results process(istream &in) {
    string line;
    getline(in, line);
    string prevline = line;
    prevline.replace(0, line.length(), line.length(), '.');
    string nextline;

    auto gears = map<tuple<int, int>, GearInfo>();

    int sum = 0;
    int ln = 0;
    for (string nextline; getline(in, nextline); ln++) {
        sum += evaluate_line(ln, prevline, line, nextline, gears);
        prevline = line;
        line = nextline;
    }

    nextline = line;
    nextline.replace(0, line.length(), line.length(), '.');

    sum += evaluate_line(ln, prevline, line, nextline, gears);

    return {sum, gears};
}
int Day3Solution::part1(std::istream &in) const {
    auto result = process(in);
    return result.sum;
}

int Day3Solution::part2(std::istream &in) const {
    auto result = process(in);
    int ret = 0;
    for (auto [pos, gear] : result.gearratios) {
        auto [r1, r2] = gear;
        ret += r1 * r2;
    }
    return ret;
}
