#include "day7/day7.hpp"

#include <set>

using namespace std;

struct CardHand {
    int twos = 0;
    int threes = 0;
    int fours = 0;
    int fives = 0;
    int sixes = 0;
    int sevens = 0;
    int eights = 0;
    int nines = 0;
    int tens = 0;
    int js = 0;
    int qs = 0;
    int ks = 0;
    int as = 0;

    CardHand(string hand) {
        for (int i = 0; i < hand.length(); i++) {
            switch (hand[i]) {
            case '2':
                twos++;
                break;
            case '3':

                threes++;
                break;
            case '4':

                fours++;
                break;
            case '5':

                fives++;
                break;
            case '6':

                sixes++;
                break;
            case '7':
                sevens++;
                break;
            case '8':
                eights++;
                break;
            case '9':
                nines++;
                break;
            case 'T':
                tens++;
                break;
            case 'J':
                js++;
                break;
            case 'Q':
                qs++;
                break;
            case 'K':
                ks++;
                break;
            case 'A':
                as++;
                break;
            }
        }
    }

    /*
     * 2: High card
     * 4: Pair
     * 5: 2 pair (i.e. 2.5)
     * 6: 3 of a kind
     * 7: Full house (i.e. 3.5)
     * 8: 4 of a kind
     * 10: 5 of a kind
     */
    int rank(bool with_jokers) const {
        auto j_count = js;
        if (with_jokers) {
            j_count = 0;
        }
        const vector<int> counts = {twos,
                                    threes,
                                    fours,
                                    fives,
                                    sixes,
                                    sevens,
                                    eights,
                                    nines,
                                    tens,
                                    j_count,
                                    qs,
                                    ks,
                                    as};
        auto initialrank = *max_element(counts.begin(), counts.end());
        if (with_jokers) {
            initialrank += js;
        }
        if (initialrank == 1 || initialrank == 4 || initialrank == 5) {
            // Singles, 4s and 5s are handled
            return initialrank * 2;
        }

        if (initialrank == 2) {
            // Check for 2 pair
            // Note: will never happen with jokers, as you'd take three of a
            // kind instead of pairing the joker with another singlet
            if (with_jokers) {
                return 4;
            }
            auto pairs = vector<int>();
            copy_if(counts.begin(),
                    counts.end(),
                    back_inserter(pairs),
                    [](int i) { return i == 2; });
            if (pairs.size() == 2) {
                return 5;
            } else {
                return 4;
            }
        }

        if (initialrank == 3) {
            // Check for full house
            int pairs_needed = 1;
            if (with_jokers && js == 1) {
                // For a standard full house, you need a triple and a pair here.
                // But with a single joker, you need two pairs and the joker to
                // have a full house.
                pairs_needed = 2;
            }
            auto pairs = vector<int>();
            copy_if(counts.begin(),
                    counts.end(),
                    back_inserter(pairs),
                    [](int i) { return i == 2; });
            if (pairs.size() == pairs_needed) {
                return 7;
            } else {
                return 6;
            }
        }
        return 0;
    }
};

vector<int> hand_map(string hand, bool with_jokers) {
    auto ret = vector<int>();
    for (int i = 0; i < hand.length(); i++) {
        switch (hand[i]) {
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':

            ret.push_back(hand[i] - '0');
            break;
        case 'T':
            ret.push_back(10);
            break;
        case 'J': {
            int val = 11;
            if (with_jokers) {
                val = 1;
            }
            ret.push_back(val);
            break;
        }
        case 'Q':

            ret.push_back(12);
            break;
        case 'K':

            ret.push_back(13);
            break;
        case 'A':

            ret.push_back(14);
            break;
        }
    }
    return ret;
}

class Hand {
  public:
    Hand(string line, bool with_jokers)
        : bid(stoi(line.substr(6))),
          hand(CardHand(line.substr(0, 5))),
          hand_str(line.substr(0, 5)),
          unsorted_vals(hand_map(line.substr(0, 5), with_jokers)),
          with_jokers(with_jokers) {}

    bool operator<(const Hand &other) const {
        if (hand.rank(with_jokers) != other.hand.rank(with_jokers)) {
            return hand.rank(with_jokers) < other.hand.rank(with_jokers);
        } else {
            return unsorted_vals < other.unsorted_vals;
        }
    }

    int bid;

    string debug_str() const {
        string ret = hand_str + " " + to_string(hand.rank(with_jokers)) + " ";
        for (auto val : unsorted_vals) {
            ret += to_string(val) + ",";
        }
        return ret;
    }

  private:
    CardHand hand;
    string hand_str;
    vector<int> unsorted_vals;
    bool with_jokers;
};

vector<Hand> parse_input_d7p1(istream &in) {
    auto ret = vector<Hand>();
    for (string line; getline(in, line);) {
        ret.push_back(Hand(line, false));
    }
    return ret;
}
vector<Hand> parse_input_d7p2(istream &in) {
    auto ret = vector<Hand>();
    for (string line; getline(in, line);) {
        ret.push_back(Hand(line, true));
    }
    return ret;
}

int Day7Solution::part1(istream &in) const {

    auto hands = parse_input_d7p1(in);
    sort(hands.begin(), hands.end());
    int sum = 0;
    for (int i = 0; i < hands.size(); i++) {
        sum += hands[i].bid * (i + 1);
    }
    return sum;
}

int Day7Solution::part2(istream &in) const {

    auto hands = parse_input_d7p2(in);
    sort(hands.begin(), hands.end());
    for (auto hand : hands) {
        cout << hand.debug_str() << endl;
    }
    int sum = 0;
    for (int i = 0; i < hands.size(); i++) {
        sum += hands[i].bid * (i + 1);
    }
    return sum;
}
