#include "day3/day3.hpp"

using namespace std;

bool is_symbol(char c) {
    return !((c >= '0' && c <= '9') || c == '.');
}

int evaluate_line(string prevline, string line, string nextline) {
    int ret = 0;
    cout << line << ":";
    for (int i = 0; i < line.length(); i++) {
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
        auto prevsub = prevline.substr(max(0, i - 1), sublen);
        auto nextsub = nextline.substr(max(0, i - 1), sublen);
        if (is_symbol(line[max(i - 1, 0)]) ||
            is_symbol(line[min(i + n_len, line.length())]) ||
            any_of(prevsub.begin(), prevsub.end(), is_symbol) ||
            any_of(nextsub.begin(), nextsub.end(), is_symbol)) {
            // Count the value
            ret += n;
            // Jump forward to the end of the number
            i += n_len - 1;
            cout << n << " ";
        } else {
            continue;
        }
    }
    cout << endl;
    return ret;
}

int Day3Solution::part1(std::istream &in) const {
    string line;
    getline(in, line);
    string prevline = line;
    prevline.replace(0, line.length(), line.length(), '.');
    string nextline;

    int sum = 0;
    for (string nextline; getline(in, nextline);) {
        sum += evaluate_line(prevline, line, nextline);
        prevline = line;
        line = nextline;
    }

    nextline = line;
    nextline.replace(0, line.length(), line.length(), '.');

    sum += evaluate_line(prevline, line, nextline);
    return sum;
}

int Day3Solution::part2(std::istream &in) const { return 0; }
