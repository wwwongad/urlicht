#include <urlicht/algorithm/lower_bound.h>
#include <iostream>
#include <string>
#include <vector>
#include <iomanip>

struct time_in_a_day_t {
    int hour{};
    int minute{};
};

constexpr int to_minutes(const time_in_a_day_t& t) {
    return t.hour * 60 + t.minute;
}

std::ostream& operator<<(std::ostream& os, const time_in_a_day_t& time) {
    os << std::setw(2) << std::setfill('0') << time.hour
       << ":" << std::setw(2) << std::setfill('0') << time.minute;
    return os;
}

struct appointment_t {
    time_in_a_day_t time{};
    std::string name;
};

int main() {
    const std::vector<appointment_t> calendar{
            {{9, 0}, "wake up"},
            {{9, 30}, "breakfast"},
            {{10, 0}, "team meeting"},
            {{13, 0}, "lunch"},
            {{14, 30}, "back to work"},
            {{18, 30}, "gym"},
            {{19, 30}, "dinner"},
            {{22, 0}, "read & sleep"}
    };

    const std::vector<time_in_a_day_t> timepoints {
        {8, 30}, {9, 50}, {12, 45},
        {18, 30}, {21, 0}, {23, 20}
    };

    for (const auto t : timepoints) {
        std::cout << "The time now is " << t << std::endl;
        auto it = urlicht::algorithm::lower_bound(calendar, t,
            [](const time_in_a_day_t& lhs, const time_in_a_day_t& rhs) {
                return to_minutes(lhs) < to_minutes(rhs);
            },
            [](const appointment_t& appointment) {
                return appointment.time;
            }
        );
        if (it != calendar.begin()) {
            const auto prev = it - 1;
            std::cout << " Right now: " << prev->name << "\n";
        }
        if (it != calendar.end()) {
            std::cout << "  Next thing to do: " << it->name << "\n\n";
        }
    }
    return 0;
}
