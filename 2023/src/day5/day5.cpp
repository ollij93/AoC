#include "day5/day5.hpp"

#include <optional>
#include <set>

using namespace std;

#define NUM_TYPE long
#define PARSE_INT(s) stol(s)

struct MapEntry {
    NUM_TYPE from;
    NUM_TYPE to;
    NUM_TYPE range;
};

struct IntersectResult;

struct NumRange {
    NUM_TYPE start;
    NUM_TYPE range;
    optional<IntersectResult> intersect(const NumRange &other) const;
};

struct IntersectResult {
    NumRange intersect;
    vector<NumRange> remainders;
};

optional<IntersectResult> NumRange::intersect(const NumRange &other) const {
    NUM_TYPE start = max(this->start, other.start);
    NUM_TYPE end =
        min(this->start + this->range - 1, other.start + other.range - 1);
    if (start > end) {
        return nullopt;
    }
    auto intersect = NumRange{start, end - start + 1};
    auto remainders = vector<NumRange>();
    auto left_remainder = NumRange{min(this->start, other.start),
                                   start - min(this->start, other.start)};
    if (left_remainder.range > 0) {
        remainders.push_back(left_remainder);
    }
    auto right_remainder = NumRange{
        max(this->start + this->range, other.start + other.range),
        max(this->start + this->range, other.start + other.range) - end};
    if (right_remainder.range > 0) {
        remainders.push_back(right_remainder);
    }

    return IntersectResult{intersect, remainders};
}

struct InputMaps {
    vector<MapEntry> seed_to_soil;
    vector<MapEntry> soil_to_fert;
    vector<MapEntry> fert_to_watr;
    vector<MapEntry> watr_to_lght;
    vector<MapEntry> lght_to_temp;
    vector<MapEntry> temp_to_humd;
    vector<MapEntry> humd_to_loc;

  private:
    static vector<NumRange> get_single(const vector<MapEntry> &map,
                                       const NumRange from) {
        for (auto entry : map) {
            auto intersect = from.intersect({entry.from, entry.range});
            if (intersect) {
                auto result = vector<NumRange>();
                result.push_back(
                    NumRange{entry.to + intersect->intersect.start - entry.from,
                             intersect->intersect.range});
                // Repeat the lookup on the remainders as they may fall within
                // other ranges.
                for (auto remainder : intersect->remainders) {
                    // Ignore the part of the remainder that is before the
                    // block we're interested in
                    if (remainder.start < from.start) {
                        continue;
                    }
                    // Ignore the part of the remainder that is after the
                    // block we're interested in
                    if (remainder.start + remainder.range >
                        from.start + from.range) {
                        continue;
                    }

                    for (auto subresult : get_single(map, remainder)) {
                        result.push_back(subresult);
                    }
                }
                return result;
            }
        }
        return {from};
    }

    static vector<NumRange> get_ranges(const vector<MapEntry> &map,
                                       const vector<NumRange> &from) {
        auto ret = vector<NumRange>();
        for (auto frm : from) {
            for (auto result : get_single(map, frm)) {
                ret.push_back(result);
            }
        }
        return ret;
    }

  public:
    vector<NumRange> get_seed(const NumRange from) {
        vector<NumRange> soil = get_single(seed_to_soil, from);
        vector<NumRange> fert = get_ranges(soil_to_fert, soil);
        vector<NumRange> watr = get_ranges(fert_to_watr, fert);
        vector<NumRange> lght = get_ranges(watr_to_lght, watr);
        vector<NumRange> temp = get_ranges(lght_to_temp, lght);
        vector<NumRange> humd = get_ranges(temp_to_humd, temp);
        return get_ranges(humd_to_loc, humd);
    }
};

InputMaps parse_input_maps(istream &in) {
    auto maps = vector<vector<MapEntry>>();

    for (string line; getline(in, line);) {
        // Ignore blank lines
        if (line.length() == 0) {
            continue;
        }
        if (line.back() == ':') {
            maps.push_back(vector<MapEntry>());
        } else {
            auto parts = split(line, " ");
            maps.back().push_back({
                // Note: the two and from numbers are flipped
                PARSE_INT(parts[1]),
                PARSE_INT(parts[0]),
                PARSE_INT(parts[2]),
            });
        }
    }

    return {
        maps[0],
        maps[1],
        maps[2],
        maps[3],
        maps[4],
        maps[5],
        maps[6],
    };
};

struct InputDataP1 {
    set<NUM_TYPE> seeds;
    InputMaps maps;
};

InputDataP1 parse_input_p1(istream &in) {
    set<NUM_TYPE> seeds;
    string line1;
    getline(in, line1);
    line1 = line1.substr(7); // "seeds: "
    for (string seed : split(line1, " ")) {
        seeds.insert(PARSE_INT(seed));
    }

    auto maps = parse_input_maps(in);
    return {seeds, maps};
};

struct InputDataP2 {
    vector<NumRange> seeds;
    InputMaps maps;
};

InputDataP2 parse_input_p2(istream &in) {
    vector<NUM_TYPE> seednums;
    string line1;
    getline(in, line1);
    line1 = line1.substr(7); // "seeds: "
    for (string seed : split(line1, " ")) {
        seednums.push_back(PARSE_INT(seed));
    }

    vector<NumRange> seeds;
    for (int i = 0; i < seednums.size(); i += 2) {
        auto seed = NumRange{seednums[i], seednums[i + 1]};
        seeds.push_back(seed);
    }

    auto maps = parse_input_maps(in);
    return {seeds, maps};
};

int Day5Solution::part1(std::istream &in) const {
    auto data = parse_input_p1(in);
    NUM_TYPE closest = INT_MAX;
    for (NUM_TYPE seed : data.seeds) {
        for (auto loc : data.maps.get_seed({seed, 1})) {
            closest = min(closest, loc.start);
        }
    }
    return (int)closest;
}

int Day5Solution::part2(std::istream &in) const {
    auto data = parse_input_p2(in);
    NUM_TYPE closest = INT_MAX;
    for (auto seed : data.seeds) {
        for (auto loc : data.maps.get_seed(seed)) {
            closest = min(closest, loc.start);
        }
    }
    return (int)closest;
}
