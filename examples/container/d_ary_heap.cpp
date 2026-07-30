#include <urlicht/container/d_ary_heap.h>

#include <iostream>
#include <string>
#include <vector>

struct ticket {
    int priority;
    std::string summary;

    friend bool operator<(const ticket& lhs, const ticket& rhs) {
        return lhs.priority < rhs.priority;
    }
};

int main() {
    using ticket_queue = urlicht::container::d_ary_heap<
        ticket, std::vector, std::less<>, {.mutable_ = true, .stable = true}>;

    ticket_queue backlog;
    const auto deployment = backlog.emplace(ticket{2, "deploy documentation"});
    backlog.emplace(ticket{5, "restore payment service"});
    backlog.modify(deployment, [](ticket& item) { item.priority = 10; });

    while (!backlog.empty()) {
        std::cout << backlog.top().summary << '\n';
        backlog.pop();
    }
}
