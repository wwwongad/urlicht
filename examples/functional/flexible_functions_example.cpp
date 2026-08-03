#include <urlicht/functional/flexible_function.h>

#include <vector>
#include <unordered_map>
#include <cstdio>
#include <iostream>

using callback_list_t = std::vector<urlicht::functional::flexible_function<void() noexcept>>;

enum class event_t {
    pressed,
    released,
    cancelled,
    submitted
};

class dispatcher {
    std::unordered_map<event_t, callback_list_t> event_queues{};
public:
    dispatcher() = default;

    template <typename Func>
    void add_callback(const event_t ev, Func&& func) {
        event_queues[ev].emplace_back(std::forward<Func>(func));
    }

    void on_event(const event_t ev) {
        for (auto& func : event_queues[ev]) {
            func();
        }
    }
};

void print_callback(const int id, const char* ev) noexcept {
    std::printf("Callback function on %s with id %d invoked\n", ev, id);
}

int main() {
    dispatcher disp;

    disp.add_callback(event_t::pressed, [id = 10, ev = "press"]() noexcept {
        print_callback(id, ev);
    });
    disp.add_callback(event_t::released, [id = 22, ev = "release"]() noexcept {
        print_callback(id, ev);
    });
    disp.add_callback(event_t::released, [id = 45, ev = "release"]() noexcept {
        print_callback(id, ev);
    });
    disp.add_callback(event_t::submitted, [id = 30, ev = "submission"]() noexcept {
        print_callback(id, ev);
    });

    std::cout << "Pressed: \n";
    disp.on_event(event_t::pressed);

    std::cout << "\nReleased: \n";
    disp.on_event(event_t::released);

    std::cout << "\nCancelled: \n";
    disp.on_event(event_t::cancelled);

    std::cout << "\nSubmitted: \n";
    disp.on_event(event_t::submitted);
}
