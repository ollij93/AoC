#include "day4/day4.hpp"

#include <map>

using namespace std;

vector<Day4Solution::Card> Day4Solution::parse_input(istream &in) const {
    vector<Card> ret;
    for (string line; getline(in, line);) {
        auto card = Card();
        line = line.substr(line.find(':') + 2);
        while (line.front() != '|') {
            int n = stoi(line);
            card.winning_nums.insert(n);
            line = line.substr(3);
        }
        line = line.substr(2);
        while (line.length() > 0) {
            int n = stoi(line);
            card.card_nums.insert(n);
            line = line.substr(min(3, (int)line.length()));
        }
        ret.push_back(card);
    }
    return ret;
}

int Day4Solution::part1(std::istream &in) const {
    auto cards = parse_input(in);
    int sum = 0;
    for (auto &card : cards) {
        int score = 0;
        for (auto &num : card.card_nums) {
            if (card.winning_nums.find(num) != card.winning_nums.end()) {
                if (score == 0) {
                    score++;
                } else {
                    score *= 2;
                }
            }
        }
        sum += score;
    }
    return sum;
}

int Day4Solution::part2(std::istream &in) const {
    auto cards = parse_input(in);
    map<int, int> cardcounts = map<int, int>();
    for (int i = 0; i < cards.size(); i++) {
        cardcounts[i] = 1;
    }

    for (int i = 0; i < cards.size(); i++) {
        auto card = cards.at(i);
        int score = 0;
        for (auto &num : card.card_nums) {
            if (card.winning_nums.find(num) != card.winning_nums.end()) {
                score++;
            }
        }

        for (int j = 0; j < score; j++) {
            cardcounts[i + 1 + j] += cardcounts[i];
        }
    }

    int sum = 0;
    for (int i = 0; i < cards.size(); i++) {
        sum += cardcounts[i];
    }
    return sum;
}
