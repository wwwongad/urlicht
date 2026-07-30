#ifndef URLICHT_CONCURRENCY_MPMC_QUEUE_H
#define URLICHT_CONCURRENCY_MPMC_QUEUE_H

#include <urlicht/config.h>
#include <urlicht/concepts/concepts.h>
#include <urlicht/concurrency/detail/utils_.h>
#include <memory>
#include <atomic>
#include <bit>
#include <optional>
#include <functional>
#include <memory_resource>

namespace urlicht::concurrency {
    /**
     * @brief A lock-free bounded MPMC queue. It has the following advantages over other implementations:
     *
     *        1. Policy-defined storage mode - supports both compile-time fixed capacity, which uses in-place
     *           storage for efficiency, and runtime dynamic size that uses the user-defined allocator.
     *
     *        2. No specific type requirements on the value type T on top of being nothrow destructible
     *           (true for most, if not all types). Nothrow move constructibility and nothrow move
     *           assignability are not strictly enforced unless you use the methods that need them (guarded by
     *           requires clause).
     *           Default-constructibility and trivial destructibility are not mandatory at all, unlike
     *           boost::lockfree::queue. This adds much flexibility to the queue.
     *
     *        3. Lambda-based [try_]consume methods allow for in-place processing of elements
     *           without the need of moving them out, which may add non-trivial overhead.
     *
     *        4. Monotonically increasing indices combined with power-of-two capacity replaces heavy branches
     *           and integer division operations with a single bit mask. Overflow would take more than 500 years
     *           with size_type = std::uint64_t.
     *
     * @tparam T The value_type of the queue. Must satisfy std::is_object_v and std::is_nothrow_destructible_v.
     * @tparam Policy Either a valid allocator type or urlicht::concurrency::capacity<N>, where N
     *                is the intended capacity of the queue. N must be a power-of-two. Defaults to std::allocator.
     *
     * @note: Under most circumstances where contention is medium-to-high, emplace is preferred over
     *        try_emplace loop (unless there is something else to do upon failure). However, if the queue
     *        is expected to be constantly full or empty due to imbalanced load, or if contention is
     *        ultra-high, a try_emplace loop is more efficient.
     */
    template <typename T, typename Policy = std::allocator<T>>
    class mpmc_queue;

    namespace detail {
        template <typename T>
        class mpmc_slot_ {
        public:
            constexpr mpmc_slot_() noexcept = delete;
            constexpr mpmc_slot_(const std::uint64_t init) noexcept : turn_{init} { }

            constexpr ~mpmc_slot_() noexcept = default; // No-op, let the queue decide whether to destroy

            template <typename ...Args>
            constexpr void construct_from(Args&& ...args) noexcept {
                std::construct_at(reinterpret_cast<T*>(this->data_), std::forward<Args>(args)...);
            }
            constexpr T&& extract() noexcept {
                return std::move(*reinterpret_cast<T*>(this->data_));
            }
            constexpr void destroy() noexcept {
                std::destroy_at(reinterpret_cast<T*>(this->data_));
            }

            // Observer
            constexpr auto& turn() noexcept { return this->turn_; }
            constexpr const auto& turn() const noexcept { return this->turn_; }
        private:
            alignas(cacheline_size_) std::atomic<std::uint64_t> turn_{};
            alignas(alignof(T)) std::byte data_[sizeof(T)]
#if UL_HAS_CPP26
            [[indeterminate]]
#endif
            ;
        };

        template <typename T, std::uint64_t Capacity>
        class mpmc_fixed_buffer_ {
            static_assert(std::has_single_bit(Capacity) && Capacity > 1, "Capacity must be a power-of-two.");
        public:
            struct empty_t {};
            using allocator_type = empty_t;  // In place of std::monostate

            [[nodiscard]] static constexpr std::uint64_t capacity() noexcept {
                return Capacity;
            }

            constexpr mpmc_fixed_buffer_() noexcept = default;
            constexpr ~mpmc_fixed_buffer_() noexcept = default;

        protected:
            constexpr mpmc_slot_<T>* data_() noexcept {
                return reinterpret_cast<mpmc_slot_<T>*>(slots_);
            }

            constexpr mpmc_slot_<T>& slot_at_(const std::uint64_t idx) noexcept {
                return data_()[idx];
            }

            // Data member
            alignas(alignof(mpmc_slot_<T>)) std::byte slots_[Capacity * sizeof(mpmc_slot_<T>)]
#if UL_HAS_CPP26
            [[indeterminate]]
#endif
            ;
        };

        template <typename T, typename Alloc>
        class mpmc_runtime_sized_buffer_ {
            static_assert(urlicht::concepts::allocator<Alloc>,  "Alloc must be a valid allocator type.");
        public:
            using allocator_type = std::allocator_traits<Alloc>::template rebind_alloc<mpmc_slot_<T>>;
            using allocator_traits = std::allocator_traits<allocator_type>;

            template <typename Alloc_ = Alloc>
            constexpr mpmc_runtime_sized_buffer_(const std::uint64_t capacity, const Alloc_& alloc = Alloc_{})
            : capacity_{capacity}, alloc_{alloc}, slots_{allocator_traits::allocate(alloc_, capacity_)} {
                if (this->capacity_ == 1 || !std::has_single_bit(this->capacity_)) [[unlikely]] {
                    allocator_traits::deallocate(alloc_, slots_, capacity_);
                    throw std::invalid_argument(
                        "urlicht::concurrency::mpmc_queue(): capacity must be a power-of-two"
                    );
                }
            }

            constexpr ~mpmc_runtime_sized_buffer_() noexcept {
                allocator_traits::deallocate(alloc_, slots_, capacity_);
            }

            [[nodiscard]] constexpr std::uint64_t capacity() const noexcept {
                return capacity_;
            }
        protected:
            constexpr mpmc_slot_<T>* data_() noexcept {
                return slots_;
            }

            constexpr mpmc_slot_<T>& slot_at_(const std::uint64_t idx) noexcept {
                return slots_[idx];
            }

            // Data member
            const std::uint64_t capacity_;
            UL_NO_UNIQUE_ADDRESS allocator_type alloc_;
            mpmc_slot_<T>* const slots_{};
        };

        template <typename T, typename A>
        struct mpmc_buffer_type_ {
            using type = mpmc_runtime_sized_buffer_<T, A>;
        };

        template <typename T, std::uint64_t N>
        struct mpmc_buffer_type_<T, urlicht::concurrency::capacity<N>> {
            using type = mpmc_fixed_buffer_<T, N>;
        };
    }

    template <typename T, typename Policy>
    class mpmc_queue : private detail::mpmc_buffer_type_<T, Policy>::type {
        static_assert(urlicht::concepts::object<T>, "T must be an object.");
        static_assert(
            std::is_nothrow_destructible_v<T>, "T must be nothrow destructible."
        );
        using buffer_type_ = detail::mpmc_buffer_type_<T, Policy>::type;
    public:
        using value_type = T;
        using size_type = std::uint64_t;
        using pointer = T*;
        using const_pointer = const T*;
        using reference = T&;
        using const_reference = const T&;
        // Note: If the queue does not use an allocator, allocator_type is set to an arbitrary non-allocator
        // type (e.g. std::monostate) instead of void. This is to avoid errors associated with using void as a
        // default template parameter and later trying to default-initialize it.
        // Such error may occur regardless of whether the templated method is instantiated.
        using allocator_type = typename buffer_type_::allocator_type;

        static constexpr std::size_t cacheline_size = detail::cacheline_size_;

        [[nodiscard]] static consteval bool is_always_lock_free() noexcept {
            return std::atomic<std::uint64_t>::is_always_lock_free;
        }

        using buffer_type_::capacity;

        mpmc_queue() noexcept
        requires detail::is_capacity_<Policy>::value {
            for (std::uint64_t idx = 0; idx < this->capacity(); ++idx) {
                std::construct_at(this->data_() + idx, idx);
            }
        }

        template <urlicht::concepts::can_construct<allocator_type> Alloc_ = allocator_type>
        requires (!detail::is_capacity_<Policy>::value)
        mpmc_queue(const std::uint64_t capacity, const Alloc_& alloc = Alloc_{})
        : buffer_type_(capacity, alloc) {
            for (std::uint64_t idx = 0; idx < this->capacity(); ++idx) {
                std::construct_at(this->data_() + idx, idx);
            }
        }

        mpmc_queue(const mpmc_queue&)               = delete;
        mpmc_queue& operator=(const mpmc_queue&)    = delete;
        mpmc_queue(mpmc_queue&&)                    = delete;
        mpmc_queue& operator=(mpmc_queue&&)         = delete;

        ~mpmc_queue() noexcept {
            for (std::uint64_t idx = 0U; idx < this->capacity(); ++idx) {
                if (auto& slot = this->slot_at_(idx);
                    normalized_(slot.turn().load(std::memory_order_acquire)) != idx) {
                    slot.destroy();
                }
                // std::destroy_at(std::addressof(slot)); // Note: this is no-op
            }
        }

        //**************** Producer methods ****************//

        /**
         * @brief Tries to emplace an element into the queue. Immediately returns false if the queue
         *        is full; keeps trying if another thread claims the current slot that might be available.
         */
        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...> &&
                 std::is_nothrow_constructible_v<value_type, Args&&...>
        [[nodiscard]] bool try_emplace(Args&& ...args) noexcept {
            auto idx = writer_idx_.load(std::memory_order_relaxed);
            while (true) {
                auto& slot = this->slot_at_(normalized_(idx));
                auto turn = slot.turn().load(std::memory_order_acquire);
                if (const std::int64_t diff = turn - idx; diff == 0) {
                    if (writer_idx_.compare_exchange_strong(idx, idx + 1, std::memory_order_relaxed)) {
                        slot.construct_from(std::forward<Args>(args)...);
                        slot.turn().store(idx + 1, std::memory_order_release);
                        return true;
                    } // else continue;
                } else if (diff < 0) {  // This slot belongs to the last turn
                    return false;
                } else { // Another thread has just taken this slot.
                    idx = writer_idx_.load(std::memory_order_relaxed);
                }
            }
        }

        [[nodiscard]] bool try_push(const_reference value) noexcept
        requires std::is_nothrow_copy_constructible_v<value_type> {
            return try_emplace(value);
        }

        [[nodiscard]] bool try_push(value_type&& value) noexcept
        requires std::is_nothrow_move_constructible_v<value_type> {
            return try_emplace(std::move(value));
        }

        /**
         * @brief Emplace an element; guaranteed to succeed by claiming the current slot and waiting
         *        until it becomes available in a busy loop.
         */
        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...> &&
                 std::is_nothrow_constructible_v<value_type, Args&&...>
        void emplace(Args&& ...args) noexcept {
            const auto idx = writer_idx_.fetch_add(1, std::memory_order_relaxed);
            auto& slot = this->slot_at_(normalized_(idx));

            while (slot.turn().load(std::memory_order_acquire) != idx) {
                detail::cpu_relax();
            }
            slot.construct_from(std::forward<Args>(args)...);
            slot.turn().store(idx + 1, std::memory_order_release);
        }

        void push(const_reference value) noexcept
        requires std::is_nothrow_copy_constructible_v<value_type> {
            emplace(value);
        }

        void push(value_type&& value) noexcept
        requires std::is_nothrow_move_constructible_v<value_type> {
            emplace(std::move(value));
        }

        //**************** Consumer methods ****************//

        [[nodiscard]] bool try_dequeue(value_type& value) noexcept
        requires std::is_nothrow_move_assignable_v<value_type> {
            return try_consume_front([&value](value_type&& top) noexcept -> void {
                value = std::move(top);
            });
        }

        [[nodiscard]] std::optional<value_type> try_dequeue() noexcept
        requires std::is_nothrow_move_constructible_v<value_type> {
            std::optional<value_type> value;
            (void) try_consume_front([&value](value_type&& top) noexcept -> void {
                value.emplace(std::move(top));
            });
            return value;
        }

        /**
         * @brief Applies the given invocable to the front element (if any) and pops the element.
         *        Returns false immediately if the queue is empty; keeps trying if another thread consumes
         *        the current slot.
         */
        template <std::invocable<value_type&&> Func>
        [[nodiscard]] bool try_consume_front(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            auto idx = reader_idx_.load(std::memory_order_relaxed);
            while (true) {
                auto& slot = this->slot_at_(normalized_(idx));
                auto turn = slot.turn().load(std::memory_order_acquire);
                if (const std::int64_t diff = turn - idx; diff == 1) {
                    if (reader_idx_.compare_exchange_strong(idx, idx + 1, std::memory_order_relaxed)) {
                        std::invoke(std::forward<Func>(func), slot.extract());
                        slot.destroy();
                        slot.turn().store(idx + this->capacity(), std::memory_order_release);
                        return true;
                    }
                } else if (diff > 1) { // Another thread has consumed this slot
                    idx = reader_idx_.load(std::memory_order_relaxed);
                } else /* if (diff < 1) */ {  // This slot hasn't been constructed yet
                    return false;
                }
            }
        }

        void dequeue(value_type& value) noexcept
        requires std::is_nothrow_move_assignable_v<value_type> {
            consume_front([&value](value_type&& top) noexcept -> void {
                value = std::move(top);
            });
        }

        [[nodiscard]] value_type dequeue() noexcept
        requires std::is_nothrow_move_constructible_v<value_type> {
            // We cannot reuse consume_front here because that would require default-initializing
            // a value here and pass it into consume_front for move assignment,
            // which leads to an extra default-initialization.
            const auto idx = reader_idx_.fetch_add(1, std::memory_order_relaxed);
            auto& slot = this->slot_at_(normalized_(idx));

            while (slot.turn().load(std::memory_order_acquire) != idx + 1) {
                detail::cpu_relax();
            }
            value_type value{slot.extract()};
            slot.destroy();
            slot.turn().store(idx + this->capacity(), std::memory_order_release);
            return value;
        }

        /**
         * @brief Consumes the front element with the given invocable. Guaranteed to succeed by claiming
         *        the current slot and waiting for its availability in a busy loop.
         */
        template <std::invocable<value_type&&> Func>
        void consume_front(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            const auto idx = reader_idx_.fetch_add(1, std::memory_order_relaxed);
            auto& slot = this->slot_at_(normalized_(idx));

            while (slot.turn().load(std::memory_order_acquire) != idx + 1) {
                detail::cpu_relax();
            }
            std::invoke(std::forward<Func>(func), slot.extract());
            slot.destroy();
            slot.turn().store(idx + this->capacity(), std::memory_order_release);
        }

        // Provides a best-effort guess of the queue size. Note that it is possible to return a negative value
        // when empty and a value greater than capacity() when full.
        [[nodiscard]] std::make_signed_t<size_type> size() const noexcept {
            return writer_idx_.load(std::memory_order_relaxed) - reader_idx_.load(std::memory_order_relaxed);
        }

        [[nodiscard]] bool empty() const noexcept {
            return size() <= 0;
        }

        [[nodiscard]] bool full() const noexcept {
            return static_cast<size_type>(size()) >= this->capacity();
        }

        /**
         * @brief Clears the queue thoroughly by destroying all existing elements and resetting all
         *        counters/indices to their initial state.
         * @warning This method should only be called quiescently.
         */
        void clear() noexcept {
            for (std::uint64_t idx = 0U; idx < this->capacity(); ++idx) {
                auto& slot = this->slot_at_(idx);
                if (normalized_(slot.turn().load(std::memory_order_acquire)) != idx) {
                    slot.destroy();
                }
                slot.turn().store(idx, std::memory_order_release);
            }
            reader_idx_.store(0, std::memory_order_release);
            writer_idx_.store(0, std::memory_order_release);
        }

    private:
        [[nodiscard]] size_type normalized_(const size_type idx) const noexcept {
            return idx & (this->capacity() - 1);
        }

        alignas(cacheline_size) std::atomic<size_type> reader_idx_{};
        alignas(cacheline_size) std::atomic<size_type> writer_idx_{};
    };

    namespace pmr {
        template <typename T>
        using mpmc_queue = urlicht::concurrency::mpmc_queue<T, std::pmr::polymorphic_allocator<T>>;
    }

    namespace detail {
        template <typename>
        struct is_mpmc_queue : std::false_type {};
        template <typename T, typename P>
        struct is_mpmc_queue<urlicht::concurrency::mpmc_queue<T, P>> : std::true_type {};
    }
}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_mpmc_queue_v = urlicht::concurrency::detail::is_mpmc_queue<T>::value;
}

#endif //URLICHT_CONCURRENCY_MPMC_QUEUE_H
