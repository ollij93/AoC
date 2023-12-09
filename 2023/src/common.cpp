#include "common.hpp"

#include <vector>

using namespace std;

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
