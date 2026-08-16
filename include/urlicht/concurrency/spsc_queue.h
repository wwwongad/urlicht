#ifndef URLICHT_CONCURRENCY_SPSC_QUEUE_H
#define URLICHT_CONCURRENCY_SPSC_QUEUE_H

#include <urlicht/internal/config.h>
#include <urlicht/concepts/concepts.h>
#include <urlicht/concurrency/detail/utils_.h>
#include <urlicht/internal/scope_guard.h>
#include <type_traits>
#include <atomic>
#include <memory>
#include <bit>
#include <optional>
#include <memory_resource>

namespace urlicht::concurrency {

    /**
     * @brief A lock-free single-producer-single-consumer queue.
     * @tparam T The value type of the queue. It must be nothrow move constructible OR nothrow move
     *           assignable.
     * @tparam Policy Either an urlicht::concurrency::capacity<N> tag for compile-time fixed size,
     *           or an allocator type for a runtime-sized queue. Defaults to std::allocator<T>.
     *           IMPORTANT: Either case, the queue size must be a power-of-two.
     *
     * @note spsc_queue uses monotonically increasing indices. If you increment them every nanosecond, they
     *       will overflow in roughly 584 years.
     */
    template <typename T, typename Policy = std::allocator<T>>
    class spsc_queue;

    namespace detail {

        template <typename T, std::uint64_t N>
        class spsc_fixed_buffer_ {
            static_assert(std::has_single_bit(N), "Capacity must be a power of two");
        public:
            [[nodiscard]] static constexpr std::uint64_t capacity() noexcept {
                return N;
            }
            constexpr spsc_fixed_buffer_() noexcept = default;
            constexpr ~spsc_fixed_buffer_() noexcept = default;

            [[nodiscard]] constexpr T* data() noexcept {
                return reinterpret_cast<T*>(buffer_);
            }
        protected:
            alignas(T) std::byte buffer_[sizeof(T) * N]
#if UL_HAS_CPP26
            [[indeterminate]]
#endif
            ;
        };

        template <typename T, typename Allocator>
        class spsc_runtime_sized_buffer_ {
            static_assert(urlicht::concepts::allocator<Allocator>, "Allocator must be a valid allocator type");
        public:
            using allocator_type = Allocator;
            using allocator_traits = std::allocator_traits<allocator_type>;

            static_assert(std::same_as<T, typename allocator_type::value_type>,
                "Mismatch of container and allocator value_types");

            constexpr spsc_runtime_sized_buffer_() noexcept = delete;  // Must provide init size

            template <urlicht::concepts::can_construct<allocator_type> Alloc = allocator_type>
            constexpr spsc_runtime_sized_buffer_(const std::uint64_t capacity, const Alloc& alloc = Alloc{})
            : capacity_{capacity}, alloc_{alloc}, buffer_{allocator_traits::allocate(alloc_, capacity_)} {
                if (!std::has_single_bit(capacity_)) [[unlikely]] {
                    allocator_traits::deallocate(alloc_, buffer_, capacity_);
                    throw std::invalid_argument(
                        "urlicht::concurrency::spsc_queue: Capacity must be a power of two"
                    );
                }
            }

            constexpr ~spsc_runtime_sized_buffer_() noexcept {
                allocator_traits::deallocate(alloc_, buffer_, capacity_);
            }

            [[nodiscard]] constexpr std::uint64_t capacity() const noexcept {
                return capacity_;
            }

            [[nodiscard]] constexpr T* data() noexcept {
                return buffer_;
            }
        protected:
            const std::uint64_t capacity_{};
            UL_NO_UNIQUE_ADDRESS allocator_type alloc_{};
            T* const buffer_{};
        };

        template <typename T, typename Policy>
        struct spsc_buffer_type_ {
            using type = spsc_runtime_sized_buffer_<T, Policy /* As allocator */>;
        };

        template <typename T, std::uint64_t N>
        struct spsc_buffer_type_<T, capacity<N>> {
            using type = spsc_fixed_buffer_<T, N>;
        };
    }


    template <typename T, typename Policy>
    class spsc_queue : private detail::spsc_buffer_type_<T, Policy>::type {
        using buffer_type = typename detail::spsc_buffer_type_<T, Policy>::type;
    public:
        static constexpr auto cacheline_size = detail::cacheline_size_;

        using value_type = T;
        using size_type = std::uint64_t;
        using pointer = T*;
        using const_pointer = const T*;
        using reference = T&;
        using const_reference = const T&;
        using allocator_type = std::conditional_t<
            detail::is_capacity_<Policy>::value,
            void,
            Policy
        >;

        static_assert(
            std::is_nothrow_move_constructible_v<value_type> || std::is_nothrow_move_assignable_v<value_type>,
            "value_type must be either nothrow move constructible or nothrow move assignable."
        );

        using buffer_type::buffer_type;

        [[nodiscard]] static consteval bool is_always_lock_free() noexcept {
            return std::atomic<size_type>::is_always_lock_free;
        }

        // For compile-time fixed size only
        spsc_queue() noexcept
            requires detail::is_capacity_<Policy>::value
        = default;

        spsc_queue(const spsc_queue&)               = delete;
        spsc_queue& operator=(const spsc_queue&)    = delete;
        spsc_queue(spsc_queue&&)                    = delete;
        spsc_queue& operator=(spsc_queue&&)         = delete;

        ~spsc_queue() noexcept {
            if constexpr (!std::is_trivially_destructible_v<value_type>) {
                pointer buffer = this->data();
                auto begin = reader_idx_.load(std::memory_order_acquire);
                for (const auto end = writer_idx_.load(std::memory_order_acquire);
                    begin != end; ++begin) {
                    std::destroy_at(buffer + normalized_(begin));
                }
            }
        }

        //**************** Producer methods ****************//

        // Performs 2 + 1/n atomic operations in average
        template <typename ...Args>
        bool try_emplace(Args&&... args)
        noexcept(std::is_nothrow_constructible_v<value_type, Args&&...>) {
            const auto curr_writer_idx = writer_idx_.load(std::memory_order_relaxed);

            // curr_writer_idx - reader_cache_ >= curr_writer_idx - reader_idx_
            if (curr_writer_idx - reader_cache_ == this->capacity()) [[unlikely]] {
                reader_cache_ = reader_idx_.load(std::memory_order_acquire);
                if (curr_writer_idx - reader_cache_ == this->capacity()) {
                    return false;
                }
            }
            std::construct_at(slot_at_(normalized_(curr_writer_idx)), std::forward<Args>(args)...);
            writer_idx_.store(curr_writer_idx + 1, std::memory_order_release);
            return true;  // successfully pushed an element
        }

        bool try_push(const_reference value)
        noexcept(std::is_nothrow_copy_constructible_v<value_type>) {
            return try_emplace(value);
        }

        bool try_push(value_type&& value)
        noexcept(std::is_nothrow_move_constructible_v<value_type>) {
            return try_emplace(std::move(value));
        }

        // Loops until the queue is not full
        template <typename ...Args>
        void emplace(Args&&... args)
        noexcept(std::is_nothrow_constructible_v<value_type, Args&&...>) {
            const auto curr_writer_idx = writer_idx_.load(std::memory_order_relaxed);

            if (curr_writer_idx - reader_cache_ == this->capacity()) [[unlikely]] {
                do {
                    detail::cpu_relax();
                    reader_cache_ = reader_idx_.load(std::memory_order_acquire);
                } while (curr_writer_idx - reader_cache_ == this->capacity());
            }
            std::construct_at(slot_at_(normalized_(curr_writer_idx)), std::forward<Args>(args)...);
            writer_idx_.store(curr_writer_idx + 1, std::memory_order_release);
        }

        void push(const_reference value)
        noexcept(std::is_nothrow_copy_constructible_v<value_type>) {
            emplace(value);
        }

        void push(value_type&& value)
        noexcept(std::is_nothrow_move_constructible_v<value_type>) {
            emplace(std::move(value));
        }

        /**
         * @warning Iterating over the given range must be a nothrow operation.
         */
        template <urlicht::concepts::compatible_range<value_type> Rng>
        requires std::ranges::sized_range<Rng>
        size_type push_range(Rng&& rng) {
            auto it = std::ranges::begin(rng);
            return push_impl_(std::ranges::size(rng), [&]() -> decltype(auto) { return *it++; });
        }

        size_type push_range(std::initializer_list<value_type> ilist) {
            auto it = std::ranges::begin(ilist);
            return push_impl_(std::ranges::size(ilist), [&]() -> decltype(auto) { return *it++; });
        }

        size_type push_n(const size_type n, const_reference value) {
            return push_impl_(n, [&]() -> const_reference { return value; });
        }

        // This is useful when the elements to be pushed can be generated lazily,
        // while avoiding an intermediate buffer which push_range would have required.
        template <typename Gen>
        requires std::invocable<Gen&> &&
                 std::constructible_from<value_type, std::invoke_result_t<Gen&>>
        size_type push_n_from(const size_type n, Gen&& gen) {
            return push_impl_(n, std::forward<Gen>(gen));
        }

        //**************** Consumer methods ****************//

        [[nodiscard]] bool try_dequeue(reference value) noexcept
        requires std::is_nothrow_move_assignable_v<value_type> {
            return try_consume_front([&](value_type&& top) noexcept {
                value = std::move(top);
            });
        }

        [[nodiscard]] std::optional<value_type> try_dequeue() noexcept
        requires std::is_nothrow_move_constructible_v<value_type> {
            std::optional<value_type> opt;
            try_consume_front([&](value_type&& top) noexcept {
                opt.emplace(std::move(top));
            });
            return opt;
        }

        template <std::invocable<value_type&&> Func>
        bool try_consume_front(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            const auto curr_reader_idx = reader_idx_.load(std::memory_order_relaxed);

            // writer_cache_ <= writer_idx_
            if (curr_reader_idx == writer_cache_) [[unlikely]] {
                writer_cache_ = writer_idx_.load(std::memory_order_acquire);
                if (curr_reader_idx == writer_cache_) {
                    return false;
                }
            }
            pointer fp = slot_at_(normalized_(curr_reader_idx));
            std::invoke(std::forward<Func>(func), std::move(*fp));
            std::destroy_at(fp);
            reader_idx_.store(curr_reader_idx + 1, std::memory_order_release);
            return true;
        }

        void dequeue(reference value) noexcept
        requires std::is_nothrow_move_assignable_v<value_type> {
            consume_front([&] (value_type&& top) noexcept {
                value = std::move(top);
            });
        }

        [[nodiscard]] value_type dequeue() noexcept
        requires std::is_nothrow_move_constructible_v<value_type> {
            // We cannot reuse consume_front here because that would require default-initializing
            // a value here and pass it into consume_front for move assignment,
            // which leads to an extra default-initialization.
            const auto curr_reader_idx = reader_idx_.load(std::memory_order_relaxed);
            if (curr_reader_idx == writer_cache_) [[unlikely]] {
                do {
                    detail::cpu_relax();
                    writer_cache_ = writer_idx_.load(std::memory_order_acquire);
                } while (curr_reader_idx == writer_cache_);
            }
            pointer fp = slot_at_(normalized_(curr_reader_idx));
            value_type val = std::move(*fp);
            std::destroy_at(fp);
            reader_idx_.store(curr_reader_idx + 1, std::memory_order_release);
            return val;
        }

        template <std::invocable<value_type&&> Func>
        void consume_front(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            const auto curr_reader_idx = reader_idx_.load(std::memory_order_relaxed);
            if (curr_reader_idx == writer_cache_) [[unlikely]] {
                do {
                    detail::cpu_relax();
                    writer_cache_ = writer_idx_.load(std::memory_order_acquire);
                } while (curr_reader_idx == writer_cache_);
            }
            pointer fp = slot_at_(normalized_(curr_reader_idx));
            std::invoke(std::forward<Func>(func), std::move(*fp));
            std::destroy_at(fp);
            reader_idx_.store(curr_reader_idx + 1, std::memory_order_release);
        }

        /**
         * @brief Applies the given callable object to the front element without
         *        consuming (popping) it.
         */
        template <std::invocable<value_type&&> Func>
        [[nodiscard]] bool try_apply_front(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            const auto curr_reader_idx = reader_idx_.load(std::memory_order_relaxed);

            if (curr_reader_idx == writer_cache_) [[unlikely]] {
                writer_cache_ = writer_idx_.load(std::memory_order_acquire);
                if (curr_reader_idx == writer_cache_) {
                    return false;
                }
            }
            pointer fp = slot_at_(normalized_(curr_reader_idx));
            std::invoke(std::forward<Func>(func), std::move(*fp));
            return true;
        }

        /**
         * @brief Waits until non-empty and applies the given callable object to the front
         *        element without consuming (popping) it.
         */
        template <std::invocable<value_type&&> Func>
        void apply_front(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            const auto curr_reader_idx = reader_idx_.load(std::memory_order_relaxed);
            if (curr_reader_idx == writer_cache_) [[unlikely]] {
                do {
                    detail::cpu_relax();
                    writer_cache_ = writer_idx_.load(std::memory_order_acquire);
                } while (curr_reader_idx == writer_cache_);
            }
            pointer fp = slot_at_(normalized_(curr_reader_idx));
            std::invoke(std::forward<Func>(func), std::move(*fp));
        }

        /**
         * @note Advancing the given output iterator must be a nothrow operation.
         */
        template <std::output_iterator<value_type> OutIt>
        size_type dequeue_all(OutIt o_it) noexcept
        // Assigning to an output iterator may use either construction (back insertor)
        // or assignment (existing elements), so we need to ensure both are noexcept.
        requires std::is_nothrow_move_assignable_v<value_type> &&
                 std::is_nothrow_move_constructible_v<value_type> {
            return consume_all([&](value_type&& value) noexcept {
                *o_it++ = std::move(value);
            });
        }

        template <std::invocable<value_type&&> Func>
        size_type consume_all(Func&& func) noexcept
        requires std::is_nothrow_invocable_v<Func&&, value_type&&> {
            auto curr_reader_idx = reader_idx_.load(std::memory_order_relaxed);
            writer_cache_ = writer_idx_.load(std::memory_order_acquire);

            if (curr_reader_idx == writer_cache_) [[unlikely]] {
                return 0U;
            }

            const auto output_size = writer_cache_- curr_reader_idx;
            for (; curr_reader_idx != writer_cache_; ++curr_reader_idx) {
                pointer fp = slot_at_(normalized_(curr_reader_idx));
                std::invoke(std::forward<Func>(func), std::move(*fp));
                std::destroy_at(fp);
            }
            reader_idx_.store(curr_reader_idx, std::memory_order_release);
            return output_size;
        }

        //**************** Capacity ****************//

        using buffer_type::capacity;

        [[nodiscard]] bool empty() const noexcept {
            return reader_idx_.load(std::memory_order_acquire) ==
                   writer_idx_.load(std::memory_order_acquire);
        }

        [[nodiscard]] size_type size() const noexcept {
            return writer_idx_.load(std::memory_order_acquire) -
                   reader_idx_.load(std::memory_order_acquire);
        }

        [[nodiscard]] bool full() const noexcept {
            return this->size() == this->capacity();
        }

        // This should only be called by the consumer or quiescently.
        void clear() noexcept {
            if constexpr (!std::is_trivially_destructible_v<value_type>) {
                auto reader_idx = reader_idx_.load(std::memory_order_relaxed);
                const auto writer_idx = writer_idx_.load(std::memory_order_acquire);

                for (; reader_idx != writer_idx; ++reader_idx) {
                    std::destroy_at(slot_at_(normalized_(reader_idx)));
                }
                reader_idx_.store(writer_idx, std::memory_order_release);
            } else {
                const auto writer_idx = writer_idx_.load(std::memory_order_acquire);
                reader_idx_.store(writer_idx, std::memory_order_release);
            }
        }

    private:
        [[nodiscard]] size_type normalized_(const size_type idx) const noexcept {
            return idx & (this->capacity() - 1);
        }

        [[nodiscard]] pointer slot_at_(const size_type idx) noexcept {
            return this->data() + idx;
        }

        template <typename Gen>
        size_type push_impl_(size_type n, Gen&& gen) {
            const auto curr_write_idx = writer_idx_.load(std::memory_order_relaxed);
            reader_cache_ = reader_idx_.load(std::memory_order_acquire);

            const auto input_size = std::min(
                n, this->capacity() - (curr_write_idx - reader_cache_)
            );
            if (input_size == 0) [[unlikely]] {
                return 0U;
            }

            size_type i{0U};
            auto clear_guard = urlicht::internal::make_scope_guard([&] {
                for (size_type j = 0; j < i; ++j) {
                    std::destroy_at(slot_at_(normalized_(curr_write_idx + j)));
                }
            });
            for (; i < input_size; ++i) {
                std::construct_at(slot_at_(normalized_(curr_write_idx + i)), gen());
            }
            clear_guard.release();

            writer_idx_.store(curr_write_idx + input_size, std::memory_order_release);
            return input_size;
        }

        alignas(cacheline_size) std::atomic<size_type> reader_idx_{};
        alignas(cacheline_size) size_type writer_cache_{};
        alignas(cacheline_size) std::atomic<size_type> writer_idx_{};
        alignas(cacheline_size) size_type reader_cache_{};
    };

    namespace pmr {
        template <typename T>
        using spsc_queue = urlicht::concurrency::spsc_queue<T, std::pmr::polymorphic_allocator<T>>;
    }

    namespace detail {
        template <typename>
        struct is_spsc_queue : std::false_type {};

        template <typename T, typename P>
        struct is_spsc_queue<urlicht::concurrency::spsc_queue<T, P>> : std::true_type {};
    }
}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_spsc_queue_v = concurrency::detail::is_spsc_queue<T>::value;
}

#endif //URLICHT_CONCURRENCY_SPSC_QUEUE_H
