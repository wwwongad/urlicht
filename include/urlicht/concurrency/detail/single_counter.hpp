#ifndef URLICHT_SINGLE_COUNTER_H
#define URLICHT_SINGLE_COUNTER_H
#include <atomic>

namespace urlicht::concurrency_detail {
    /**
     * @brief A minimal static single counter that keeps track of the next value.
     */
    class single_counter {
        static inline std::atomic_size_t next_idx{};
    public:
        static auto get_next() noexcept {
            return next_idx.fetch_add(1, std::memory_order_relaxed);
        }

        static auto reset() noexcept {
            next_idx.store(0, std::memory_order_relaxed);
        }
    };
}
#endif //URLICHT_SINGLE_COUNTER_H
