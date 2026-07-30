#include <urlicht/algorithm/lower_bound.h>

#include <iostream>
#include <string>
#include <vector>

struct appointment {
    int start_minutes;
    std::string title;
};

int main() {
    const std::vector<appointment> calendar{
        {9 * 60, "stand-up"}, {10 * 60 + 30, "design review"}, {14 * 60, "demo"}
    };

    const int arrival = 10 * 60;
    const auto next = urlicht::algorithm::lower_bound(
        calendar, arrival, std::less<>{}, &appointment::start_minutes
    );

    if (next != calendar.end()) {
        std::cout << "Next appointment: " << next->title << '\n';
    }
}
