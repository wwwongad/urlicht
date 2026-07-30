#ifndef URLICHT_TEST_CONCURRENCY_UTILS_TRACKED_INT_H
#define URLICHT_TEST_CONCURRENCY_UTILS_TRACKED_INT_H
#include <atomic>

class tracked_int {
    int val{};
public:
    static inline std::atomic<std::size_t> constructed{};
    static inline std::atomic<std::size_t> destructed{};

    tracked_int() noexcept { ++constructed; }
    tracked_int(const int v) noexcept : val(v) { ++constructed; }
    tracked_int(const tracked_int&) = delete;
    tracked_int& operator=(const tracked_int&) = delete;
    tracked_int(tracked_int&& other) noexcept : val{other.val} { ++constructed; }
    tracked_int& operator=(tracked_int&& other) noexcept { val = other.val; return *this; }
    ~tracked_int() { ++destructed; }

    int value() const noexcept { return val; }
};

inline bool check_lifetime() {
    return tracked_int::constructed.load() == tracked_int::destructed.load();
}


#endif //URLICHT_TEST_CONCURRENCY_UTILS_TRACKED_INT_H
