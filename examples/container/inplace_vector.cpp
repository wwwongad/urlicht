#include <urlicht/container/inplace_vector.h>

#include <iostream>
#include <ranges>
#include <stack>

int main() {
    using recent_pages = urlicht::container::inplace_vector<int, 8>;
    std::stack<int, recent_pages> history;

    history.push(101);
    history.push(205);
    history.push(318);
    std::cout << "Current page: " << history.top() << '\n';
    history.pop();

    recent_pages pages{205, 101};
    std::ranges::sort(pages);
    std::cout << "Oldest page id: " << pages.front() << '\n';
}
