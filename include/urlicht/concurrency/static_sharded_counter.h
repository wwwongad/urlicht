#ifndef URLICHT_CONCURRENCY_STATIC_SHARDED_COUNTER_H
#define URLICHT_CONCURRENCY_STATIC_SHARDED_COUNTER_H

#include <urlicht/config.h>
#include <urlicht/concurrency/detail/utils_.h>
#include <bit>
#include <thread>
#include <concepts>
#include <array>
#include <functional>
#include <type_traits>

namespace urlicht::concurrency {
    /**
     * @brief Sharded counter that maintains a fixed number of local counters for each thread or each
     *        group of threads.
     * @tparam T Integral type of the counter. Defaults to size_t.
     * @tparam Threshold When the value of the local counter exceeds {Threshold}, it is flushed to the
     *         global counter. Defaults to 1024.
     * @tparam NumLocalCnt The number of local counters. If the number of threads exceeds {NumLocalCnt}, a local
     *         counter may be mapped to by more than one thread.
     *
     * @note std::memory_order_relaxed is used across all operations because there is no dependency within the
     *       counter. If you use it for synchronization, however, you may need to change the memory orders yourself.
     */
    template <std::integral T = std::size_t,
              T Threshold = 1024u,
              std::size_t NumLocalCnt = 8u>
    class static_sharded_counter {
    public:
        using value_type = T;
        using size_type = T;

        static constexpr auto cacheline_size = detail::cacheline_size_;
    private:
        // Static next index counter
        alignas(cacheline_size) static inline constinit std::atomic_size_t global_next_idx{};

        struct alignas(cacheline_size) local_counter {
            std::atomic<value_type> count{};
        };

        // Data members
        alignas(cacheline_size) std::atomic<value_type> global_counter_{};
        alignas(cacheline_size) std::array<local_counter, NumLocalCnt> local_counters_{};

        static auto get_thread_idx_() noexcept {
            thread_local auto this_thread_idx = global_next_idx.fetch_add(1, std::memory_order_relaxed);
            if constexpr (std::has_single_bit(NumLocalCnt)) {
                return this_thread_idx & (NumLocalCnt - 1);
            } else {
                return this_thread_idx % NumLocalCnt;
            }
        }

        auto& get_counter_() noexcept {
            return local_counters_[get_thread_idx_()];
        }

        const auto& get_counter_() const noexcept {
            return local_counters_[get_thread_idx_()];
        }

        void flush_counter_(local_counter& counter) noexcept {
            const auto curr = counter.count.exchange(0, std::memory_order_relaxed);
            if (curr != 0) [[likely]] {
                global_counter_.fetch_add(curr, std::memory_order_relaxed);
            }
        }

    public:
        /**
         * @brief The threshold for flushing local value to the global counter.
         */
        static consteval value_type threshold() noexcept {
            return Threshold;
        }

        /**
         * @brief Number of local counters
         */
        static consteval std::size_t num_local_cnt() noexcept {
            return NumLocalCnt;
        }

        static consteval value_type max_size() noexcept {
            return std::numeric_limits<value_type>::max();
        }

        static void reset_global_index() noexcept {
            global_next_idx.store(0, std::memory_order_relaxed);
        }

        constexpr static_sharded_counter() noexcept = default;

        explicit constexpr static_sharded_counter(const value_type init_val) noexcept
        : global_counter_{init_val} { }

        constexpr static_sharded_counter(const static_sharded_counter&) noexcept = delete;
        constexpr static_sharded_counter(static_sharded_counter&&) noexcept = delete;
        constexpr static_sharded_counter& operator=(const static_sharded_counter&) noexcept = delete;
        constexpr static_sharded_counter& operator=(static_sharded_counter&&) noexcept = delete;

        constexpr ~static_sharded_counter() noexcept = default;

        /**
         * @brief Increments the counter by the given value {diff} (defaults to 1).
         */
        void increment(const value_type diff = 1) noexcept {
            auto& counter = this->get_counter_();
            const auto old_val = counter.count.fetch_add(diff, std::memory_order_relaxed);

            UL_ASSERT(old_val <= 0 || diff <= (max_size() - old_val), "An overflow occured");

            const auto new_val = old_val + diff;
            if (new_val >= Threshold) [[unlikely]] {
                this->flush_local();
            }
            if constexpr (std::signed_integral<value_type>) {
                if (new_val <= -Threshold) [[unlikely]] {
                    this->flush_local();
                }
            }
        }

        void flush_local() noexcept {
            this->flush_counter_(this->get_counter_());
        }

        void flush_all() noexcept {
            for (auto& counter : this->local_counters_) {
                this->flush_counter_(counter);
            }
        }

        /**
         * @note: This may not be entirely accurate; by the time a local counter is flushed, its value
         *        may no longer satisfy {pred}.
         */
        template <std::invocable<value_type> Pred>
        void flush_if(Pred&& pred) noexcept(std::is_nothrow_invocable_v<Pred, value_type>) {
            for (auto& counter : this->local_counters_) {
                if (std::invoke(pred, counter.count.load(std::memory_order_relaxed))) {
                    this->flush_counter_(counter);
                }
            }
        }

        void set(value_type val) noexcept {
            for (auto& counter : this->local_counters_) {
                counter.count.store(0, std::memory_order_relaxed);
            }
            this->global_counter_.store(val, std::memory_order_relaxed);
        }

        void reset() noexcept {
            this->set(value_type{0});
        }

        /**
         * @brief Returns the current value of the global counter.
         */
        value_type get_approximate() const noexcept {
            return this->global_counter_.load(std::memory_order_relaxed);
        }

        /**
         * @brief Returns the exact value by flushing all local counters before loading the global counter.
         */
        value_type get_exact() noexcept {
            this->flush_all();
            return this->global_counter_.load(std::memory_order_relaxed);
        }

        value_type get_exact() const noexcept {
            auto val = this->get_approximate();
            for (auto& counter : this->local_counters_) {
                val += counter.count.load(std::memory_order_relaxed);
            }
            return val;
        }
    };

    // CTAD Guides
    static_sharded_counter() -> static_sharded_counter<>;

    template <std::integral T>
    static_sharded_counter(T) -> static_sharded_counter<T>;

    namespace detail {
        template <typename>
        struct is_static_sharded_counter : std::false_type {};
        template <typename T, T Thred, std::size_t N>
        struct is_static_sharded_counter<static_sharded_counter<T, Thred, N>> : std::true_type {};
    }
}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_static_sharded_counter_v =
        concurrency::detail::is_static_sharded_counter<T>::value;
}
#endif //URLICHT_CONCURRENCY_STATIC_SHARDED_COUNTER_H
