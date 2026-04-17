#ifndef URLICHT_PERCPU_COUNTER_H
#define URLICHT_PERCPU_COUNTER_H

#include <urlicht/concurrency/detail/single_counter.h>
#include <thread>
#include <concepts>
#include <array>
#include <functional>

namespace urlicht {

    /**
     * @brief Thread-cached counter that maintains local counters for each thread or each group of threads.
     * @tparam T Integral type of the counter. Defaults to size_t.
     * @tparam Threshold When the value of the local counter exceeds {Threshold}, it is flushed to the
     *         global counter. Defaults to 1024.
     * @tparam NumLocalCnt The number of local counters. If the number of threads exceeds {NumLocalCnt}, a local
     *         counter may be mapped to by more than one thread.
     */
    template <std::integral T = size_t,
              T Threshold = 1024u,
              size_t NumLocalCnt = 8u>
    class percpu_counter {
    public:
        using value_type = T;
        using size_type = T;
    private:
        struct alignas(64) local_counter {
            std::atomic<value_type> count{};
        };

        // Data members
        alignas(64) std::atomic<value_type> global_counter{};
        std::array<local_counter, NumLocalCnt> local_counters{};

        static auto get_thread_idx() noexcept {
            thread_local auto this_thread_idx = concurrency_detail::single_counter::get_next();
            return this_thread_idx % NumLocalCnt;
        }

        auto& get_counter() noexcept {
            return local_counters[get_thread_idx()];
        }

        const auto& get_counter() const noexcept {
            return local_counters[get_thread_idx()];
        }

        void flush_counter(local_counter& counter) noexcept {
            const auto curr = counter.count.exchange(0, std::memory_order_acquire);
            if (curr != 0) [[likely]] {
                global_counter.fetch_add(curr, std::memory_order_relaxed);
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
        static consteval size_type num_local_cnt() noexcept {
            return NumLocalCnt;
        }

        constexpr percpu_counter() noexcept = default;

        explicit constexpr percpu_counter(const value_type init_val) noexcept
        : global_counter{init_val} { }

        constexpr percpu_counter(const percpu_counter&) noexcept = delete;
        constexpr percpu_counter(percpu_counter&&) noexcept = delete;
        constexpr percpu_counter& operator=(const percpu_counter&) noexcept = delete;
        constexpr percpu_counter& operator=(percpu_counter&&) noexcept = delete;

        constexpr ~percpu_counter() noexcept = default;

        /**
         * @brief Increments the counter by the given value {diff} (defaults to 1).
         */
        void increment(const value_type diff = 1) noexcept {
            auto& counter = this->get_counter();
            const auto new_val = counter.count.fetch_add(diff, std::memory_order_relaxed) + diff;

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
            this->flush_counter(this->get_counter());
        }

        void flush_all() noexcept {
            for (auto& counter : this->local_counters) {
                this->flush_counter(counter);
            }
        }

        /**
         * @note: This may not be entirely accurate; by the time a local counter is flushed, its value
         *        may no longer satisfy {pred}.
         */
        template <std::invocable<value_type> Pred>
        void flush_if(Pred&& pred) noexcept(std::is_nothrow_invocable_v<Pred, value_type>) {
            for (auto& counter : this->local_counters) {
                if (std::invoke(pred, counter.count.load(std::memory_order_acquire))) {
                    this->flush_counter(counter);
                }
            }
        }

        void set(value_type val) noexcept {
            for (auto& counter : this->local_counters) {
                counter.count.store(0, std::memory_order_release);
            }
            this->global_counter.store(val, std::memory_order_release);
        }

        void reset() noexcept {
            this->set(value_type{0});
        }

        /**
         * @brief Returns the current value of the global counter.
         */
        value_type get_approximate() const noexcept {
            return this->global_counter.load(std::memory_order_relaxed);
        }

        value_type get_exact() noexcept {
            this->flush_all();
            return this->global_counter.load(std::memory_order_acquire);
        }

        value_type get_exact() const noexcept {
            auto val = this->get_approximate();
            for (auto& counter : this->local_counters) {
                val += counter.count.load(std::memory_order_acquire);
            }
            return val;
        }
    };

    // CTAD Guides
    percpu_counter() -> percpu_counter<>;

    template <std::integral T>
    percpu_counter(T) -> percpu_counter<T>;
}

#endif //URLICHT_PERCPU_COUNTER_H
