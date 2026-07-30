#include <urlicht/algorithm/upper_bound.h>

#include <iostream>
#include <vector>

int main() {
    const std::vector<int> sorted_scores{42, 55, 55, 55, 71, 88};
    const int score = 55;

    const auto after_matching_scores = urlicht::algorithm::upper_bound(sorted_scores, score);
    const auto rank_after_ties = after_matching_scores - sorted_scores.begin();

    std::cout << rank_after_ties << " candidates scored at most " << score << '\n';
}
