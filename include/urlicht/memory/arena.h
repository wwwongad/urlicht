#ifndef URLICHT_ARENA_H
#define URLICHT_ARENA_H

#include <urlicht/config.h>
#include <urlicht/concepts_utility.h>
#include <memory>
#include <cstdint>
#include <cstddef>
#include <cassert>
#include <limits>
#include <variant>
#include <bit>

namespace urlicht {
    namespace memory_detail {

        // Chunk footer (metadata) is placed at the end of the buffer, as follows:
        //  | start               <----  curr              | &chunk_footer
            ////////////////////////////////////////////////////////////////////////
            ///                   user data                |     chunk_footer    ///
            ////////////////////////////////////////////////////////////////////////

        struct initial_buffer {
            std::byte* start;
            std::byte* curr;
            std::size_t size;
            bool external;

            constexpr std::byte* end() const noexcept {
                return start + size;
            }
        };

        struct chunk_footer {
            chunk_footer* next;
            std::byte* start;
            std::byte* curr;
            std::size_t allocation_size;

            constexpr auto actual_buffer_size() const noexcept {
                return std::bit_cast<const std::byte*>(this) - start;
            }
        };

    }

    struct ArenaGrowthPolicy {
        size_t initial_size = 1024;
        double growth_rate = 1.2;
    };

    /**
     * @brief An arena for raw memory allocation with optional upstream fallback. Behavior summary:
     *      - Allocations first consume memory from an initial buffer.
     *
     *      - When the initial buffer cannot satisfy a request and UseUpstream == true, the
     *        arena allocates additional chunks from the upstream allocator according to the growth policy.
     *
     *      - Individual de-allocations are no-ops; call release() to free all heap-allocated chunks
     *        (and the internally-allocated initial buffer, if any).
     *
     * @tparam UseUpstream Whether to fall back to upstream memory resource when initial buffer is insufficient.
     * @tparam UpstreamAlloc Upstream allocator for fall-back allocation, and for initial allocation if
     *         buffer is not provided in the constructor.
     * @tparam GrowthPolicy Defines the initial size and the growth rate of the fall-back allocations once
     *         initial buffer is insufficient. Ignored if UseUpstream is false.
     *
     * @note: - This class is typeless and thus incompatible with std::allocator_traits. Use urlicht::arena_allocator
     *          instead for STL-compatibility.
     * @note: - This class is not thread-safe.
     * @note: - If providing an external buffer to the class as the initial buffer, it will not be freed in
     *          release()/destructor, and it is the user's responsibility to manage it.
     */
    template <bool UseUpstream = true,
              ArenaGrowthPolicy GrowthPolicy = ArenaGrowthPolicy{},
              concepts::allocator UpstreamAlloc = std::allocator<std::byte>>
    class arena {
        static_assert(std::same_as<typename UpstreamAlloc::value_type, std::byte>,
            "Upstream allocator must allocate std::byte");
    public:
        using upstream_allocator = UpstreamAlloc;
        using upstream_traits = std::allocator_traits<upstream_allocator>;
    private:
        // Growth policy
        static constexpr size_t initial_chunk_size_ = GrowthPolicy.initial_size;
        static constexpr double chunk_growth_rate_ = GrowthPolicy.growth_rate;
        static constexpr size_t default_align = alignof(std::max_align_t);

        // Data members
        memory_detail::initial_buffer initial_buffer_;
        UL_NO_UNIQUE_ADDRESS std::conditional_t<UseUpstream,
            memory_detail::chunk_footer*, std::monostate> chunk_footer_{};
        UL_NO_UNIQUE_ADDRESS upstream_allocator upstream_;

        // Helper methods
        template <bool IsInitial, typename Chunk>
        static constexpr void* try_alloc_(Chunk& chunk, const size_t bytes, const size_t align) noexcept {
            if constexpr (IsInitial) {
                if (chunk.start == nullptr) [[unlikely]] {
                    return nullptr;
                }
            }
            if (const auto addr = reinterpret_cast<uintptr_t>(chunk.curr); addr < bytes) [[unlikely]] {
                return nullptr;
            }
            auto* new_curr =
                reinterpret_cast<std::byte*>(reinterpret_cast<uintptr_t>(chunk.curr - bytes) & ~(align - 1));

            if (new_curr < chunk.start) [[unlikely]] {
                return nullptr;
            }
            return chunk.curr = new_curr;
        }

        template <typename Chunk>
        static constexpr void* unchecked_alloc_(Chunk& chunk, const size_t bytes, const size_t align) noexcept {
            auto* new_curr =
                reinterpret_cast<std::byte*>(reinterpret_cast<uintptr_t>(chunk.curr - bytes) & ~(align - 1));

            return chunk.curr = new_curr;
        }

    public:
        /**
         * @brief Returns a boolean flag that indicates whether the arena falls back to upstream memory resource
         *        if the initial buffer cannot satisfy a request.
         */
        static consteval bool use_upstream() noexcept { return UseUpstream; }

        /**
         * @brief Returns the growth policy of the heap chunks.
         */
        static consteval ArenaGrowthPolicy growth_policy() noexcept requires (UseUpstream) {
            return GrowthPolicy;
        }

        /**
         * @brief Default constructor.
         */
        constexpr arena() noexcept
            : arena(nullptr, 0U, upstream_allocator{}) {}

        /**
         * @brief Constructs by allocating an initial buffer of the given size internally using a default-initialized
         *        upstream_allocator.
         * @param buffer_size Number of bytes of the initial buffer.
         */
        constexpr explicit arena(size_t buffer_size)
            : arena(nullptr, buffer_size, upstream_allocator{}) {}

        /**
         * @brief Wraps an externally provided memory of the given size as the initial buffer.
         * @param buffer Pointer to the provided buffer.
         * @param buffer_size Number of bytes of the external buffer.
         */
        constexpr arena(void* buffer, size_t buffer_size)
            : arena(buffer, buffer_size, upstream_allocator{}) {}

        /**
         * @brief Constructs with a provided allocator.
         * @param upstream Provided allocator instance for subsequent internal allocations.
         * @note This leaves the initial buffer empty. Generally, avoid using this in practice.
         */
        constexpr explicit arena(upstream_allocator upstream)
            : arena(nullptr, 0U, std::move(upstream)) {}

        /**
         * @brief Constructs by allocating an initial buffer of the given size internally using the provided
         *        upstream_allocator.
         * @param buffer_size Number of bytes of the initial buffer.
         * @param upstream Provided allocator instance for subsequent internal allocations.
         */
        constexpr arena(size_t buffer_size, upstream_allocator upstream)
            : arena(nullptr, buffer_size, std::move(upstream)) {}

        /**
         * @brief Construct with either an external or internal initial buffer and an upstream allocator.
         *
         * If buffer is nullptr and buffer_size > 0, the constructor allocates the initial buffer of the given
         * size from the provided upstream allocator. If buffer is non-null, the arena wraps the external buffer
         * and will not free it on release()/destructor.
         *
         * @param buffer pointer to external buffer or nullptr to allocate one.
         * @param buffer_size number of bytes of the initial buffer.
         * @param upstream Provided allocator instance for subsequent internal allocations.
         */
        constexpr arena(void* buffer, const size_t buffer_size, upstream_allocator upstream)
            : upstream_(std::move(upstream)) {
            if (buffer == nullptr) [[unlikely]] {
                if (buffer_size != 0U) [[likely]] {
                    initial_buffer_.start = upstream_traits::allocate(upstream_, buffer_size);
                    initial_buffer_.curr = initial_buffer_.start + buffer_size;
                } else {
                    initial_buffer_.start = nullptr;
                    initial_buffer_.curr = nullptr;
                }
                initial_buffer_.external = false;
            } else {
                initial_buffer_.start = static_cast<std::byte*>(buffer);
                initial_buffer_.curr = initial_buffer_.start + buffer_size;
                initial_buffer_.external = true;
            }
            initial_buffer_.size = buffer_size;
        }

        /**
         * @brief Copy constructor. Deleted.
         */
        constexpr arena(const arena&) = delete;

        /**
         * @brief Move constructor. Transfers ownership entirely, including the initial buffer and all
         *        subsequent heap chunks. The moved from arena is set empty.
         * @throw Nothing if upstream_allocator is nothrow move constructible.
         */
        constexpr arena(arena&& other)
        noexcept(std::is_nothrow_move_constructible_v<upstream_allocator>)
            : initial_buffer_(other.initial_buffer_),
              chunk_footer_(other.chunk_footer_),
              upstream_(std::move(other.upstream_)) {
            other.initial_buffer_ = {nullptr, nullptr, 0U, false};
            if constexpr (UseUpstream) {
                other.chunk_footer_ = nullptr;
            }
        }

        /**
         * @brief Copy assignment operator. Deleted.
         */
        constexpr arena& operator=(const arena&) = delete;

        /**
         * @brief Move assignment operator. Transfers ownership entirely, including the initial buffer and all
         *        subsequent heap chunks. The moved from arena is set empty.
         * @throw Nothing if upstream_allocator is nothrow move assignable.
         */
        constexpr arena& operator=(arena&& other)
        noexcept(std::is_nothrow_move_assignable_v<upstream_allocator>) {
            if (this != &other) [[likely]] {
                initial_buffer_ = other.initial_buffer_;
                chunk_footer_ = other.chunk_footer_;
                upstream_ = std::move(other.upstream_);
                other.initial_buffer_ = {nullptr, nullptr, 0U, false};
                if constexpr (UseUpstream) {
                    other.chunk_footer_ = nullptr;
                }
            }
            return *this;
        }

        /**
         * @brief Destructor. Calls release() to free any internally allocated buffers.
         *        External buffers are not freed.
         */
        constexpr ~arena() noexcept {
            release();
        }

        /**
         * @brief Returns a const reference to the upstream allocator.
         */
        [[nodiscard]] constexpr const auto& get_upstream() const noexcept {
            return upstream_;
        }

        [[nodiscard]] constexpr const auto& get_initial_buffer() const noexcept {
            return initial_buffer_;
        }

        [[nodiscard]] constexpr const auto* get_chunk_footer() const noexcept
        requires (UseUpstream) {
            return chunk_footer_;
        }

        /************************* CORE METHODS ****************************/

        /**
         * @brief Release all internally allocated memory, which may include the initial buffer. If the
         *        initial buffer is externally provided, set current_position to its initial value.
         */
        constexpr void release() noexcept {
            if (initial_buffer_.start) [[likely]] {
                if (initial_buffer_.external) {
                    initial_buffer_.curr = initial_buffer_.start + initial_buffer_.size;
                } else {
                    upstream_traits::deallocate(upstream_, initial_buffer_.start, initial_buffer_.size);
                    initial_buffer_ = {nullptr, nullptr, 0, false};
                }
            }
            if constexpr (UseUpstream) {
                while (chunk_footer_ != nullptr) {
                    auto* next = chunk_footer_->next;
                    upstream_traits::deallocate(
                        upstream_, chunk_footer_->start, chunk_footer_->allocation_size);
                    chunk_footer_ = next;
                }
            }
        }

        /**
         * @brief Resets initial_buffer and the last heap chunk (if UseUpstream == true) so that they
         *        become fully reusable. This does not free any memory.
         * @note: Ensure that all non-trivially destructible objects living on the memory to be reset
         *        have been destroyed.
         */
        constexpr void reset() noexcept {
            if (initial_buffer_.start) [[likely]] {
                initial_buffer_.curr = initial_buffer_.start + initial_buffer_.size;
            }
            if constexpr (UseUpstream) {
                if (chunk_footer_ != nullptr) {
                    chunk_footer_->curr = chunk_footer_->start + chunk_footer_->actual_buffer_size();
                }
            }
        }

        /**
         * @brief Perform an unconditional allocation from the initial buffer.
         *
         * @param bytes number of bytes to allocate.
         * @param align alignment requirement for the allocation, defaults to alignof(max_align_t).
         * @return void pointer to the allocated memory.
         *
         * @note UB if initial_buffer_.start == nullptr or the buffer is undersized.
         */
        [[nodiscard]]
        constexpr void* unchecked_allocate_initial(const size_t bytes,
                                                   const size_t align = default_align) noexcept {
            UL_ASSERT(align != 0U && (align & (align - 1)) == 0, "align must be non-zero power of two.");
            return unchecked_alloc_(initial_buffer_, bytes, align);
        }

        /**
         * @brief Allocates memory of the given size with the given alignment. It first attempts to allocate
         *        from the initial buffer. If failed and UseUpstream == true, it tries to allocate from the current
         *        chunk (if any). If that fails again, it creates a new chunk with sufficient memory from
         *        upstream_allocator, and then allocates from it.
         *
         * @param bytes Number of bytes requested.
         * @param align Alignment requirement. Defaults to alignof(std::max_align_t).
         * @return void pointer to the allocated memory on success, nullptr on failure.
         */
        [[nodiscard]] constexpr void* allocate(size_t bytes, const size_t align = default_align)
        noexcept(!UseUpstream) {
            UL_ASSERT(align != 0U && (align & (align - 1)) == 0, "align must be non-zero power of two.");

            if (void* res = try_alloc_<true>(initial_buffer_, bytes, align)) {
                return res;
            }

            if constexpr (UseUpstream) { // fall-back to upstream resource
                constexpr auto footer_size = sizeof(memory_detail::chunk_footer);
                constexpr auto footer_align = alignof(memory_detail::chunk_footer);

                if (chunk_footer_ != nullptr) {
                    if (void* result = try_alloc_<false>(*chunk_footer_, bytes, align)) {
                        return result;
                    }
                }

                auto get_next_default_size = [&] {
                    return chunk_footer_ ?
                        static_cast<size_t>(chunk_footer_->actual_buffer_size() * chunk_growth_rate_) :
                        initial_chunk_size_;
                };

                // Overflow prevention
                auto bad_size_ = [&](const size_t size__) {
                    return size__ > std::numeric_limits<size_t>::max()
                                    - align
                                    - footer_size
                                    - footer_align + 2;
                };

                if (bad_size_(bytes)) [[unlikely]] {
                    throw std::bad_array_new_length{};
                }
                const size_t logical_next_size =
                    std::max(bytes + align - 1, get_next_default_size()) + footer_size;
                const size_t raw_alloc_size = logical_next_size + (footer_align - 1);

                auto* raw_start = static_cast<std::byte*>(upstream_traits::allocate(upstream_, raw_alloc_size));
                auto* raw_end = raw_start + raw_alloc_size;

                const uintptr_t raw_end_addr = reinterpret_cast<uintptr_t>(raw_end - footer_size);
                const uintptr_t footer_addr = raw_end_addr & ~(static_cast<uintptr_t>(footer_align) - 1);

                auto* footer = reinterpret_cast<memory_detail::chunk_footer*>(footer_addr);
                footer->next = chunk_footer_;
                footer->start = raw_start;
                footer->curr = reinterpret_cast<std::byte*>(footer_addr);
                footer->allocation_size = raw_alloc_size;
                chunk_footer_ = footer;

                return unchecked_alloc_(*chunk_footer_, bytes, align);
            } else { // Not using upstream resource
                return nullptr;
            }
        }

        /**
         * @brief No-op de-allocation callback, for API compatibility only.
         */
        static constexpr void deallocate(void*, size_t, size_t align = default_align) noexcept { }

        friend constexpr bool operator==(const arena& lhs, const arena& rhs) noexcept {
            return std::addressof(lhs) == std::addressof(rhs);
        }

    };

    namespace memory_detail {
        template <typename T>
        struct is_arena : std::false_type {};

        template <bool U, ArenaGrowthPolicy P, typename A>
        struct is_arena<arena<U, P, A>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_arena_v = memory_detail::is_arena<T>::value;

    /////////////////////////////////////
    ///      urlicht::arena_view      ///
    /////////////////////////////////////

    /**
     * @brief A non-owning view to an allocator instance, designed to enable the sharing of the same allocator
     *        among multiple objects. It is compatible with std::allocator_traits.
     * @tparam T
     * @note Ensure that the underlying arena outlives all arena_views pointing to it.
     */
    template <concepts::object T, bool UnsafeAllocInit = false, typename Arena = arena<>>
    class arena_view;

    namespace memory_detail {
        template <typename T>
        struct is_arena_view : std::false_type {};

        template <typename T, bool I, typename A>
        struct is_arena_view<arena_view<T, I, A>> : std::true_type {};

        template <typename Arena, bool UnsafeAllocInit>
        using zero_overhead_arena =
            std::conditional_t<UnsafeAllocInit,
                               arena<false, {}, typename Arena::upstream_allocator>,
                               Arena>;
    }

    template <typename T>
    inline constexpr bool is_urlicht_arena_view_v = memory_detail::is_arena_view<T>::value;

    template <concepts::object T, bool UnsafeAllocInit, typename Arena>
    class arena_view {
        static_assert(is_urlicht_arena_v<Arena>, "Arena should be an instantiation of urlicht::arena");
    public:
        using arena_type = Arena;
    private:
        // Data member
        arena_type* ptr_arena_{};
    public:
        using value_type = T;
        using size_type = size_t;
        using difference_type = std::ptrdiff_t;
        using propagate_on_container_copy_assignment = std::true_type;
        using propagate_on_container_move_assignment = std::true_type;
        using propagate_on_container_swap = std::true_type;

        template <typename U>
        struct rebind {
            using other = arena_view<U, UnsafeAllocInit, Arena>;
        };

        /**
         * @brief Returns the UnsafeAllocInit template parameter.
         */
        static consteval bool unsafe_alloc_init() noexcept {
            return UnsafeAllocInit;
        }

        constexpr arena_view() noexcept = delete;

        /**
         * @brief Constructs with an urlicht::arena by holding a reference to it.
         * @param arena An urlicht::arena instance from which arena_view will allocate memory subsequently.
         */
        constexpr arena_view(arena_type& arena) noexcept
            : ptr_arena_(&arena) {
        }

        constexpr arena_view(const arena_view&) noexcept = default;
        constexpr arena_view(arena_view&&) noexcept = default;

        template <typename U>
        constexpr arena_view(const arena_view<U, UnsafeAllocInit, Arena>& other) noexcept
        : ptr_arena_(other.get_arena()) {
            UL_ASSERT(ptr_arena_ != nullptr, "Pointer to arena must not be null");
        }

        constexpr arena_view& operator=(const arena_view&) noexcept = default;
        constexpr arena_view& operator=(arena_view&&) noexcept = default;

        constexpr ~arena_view() noexcept = default;

        // Core Methods
        [[nodiscard]] constexpr value_type* allocate(const size_type n)
        noexcept(UnsafeAllocInit) {
            UL_ASSUME(ptr_arena_ != nullptr);
            if constexpr (UnsafeAllocInit) {
                auto* raw_bytes =
                    ptr_arena_->unchecked_allocate_initial(
                        n * sizeof(value_type), alignof(value_type)
                    );
                return static_cast<value_type*>(raw_bytes);
            } else {
                auto bad_size = [](const size_type size) {
                    return size > std::numeric_limits<size_type>::max() / sizeof(value_type);
                };
                if (bad_size(n)) [[unlikely]] {
                    throw std::bad_array_new_length{};
                }
                auto* raw_bytes = ptr_arena_->allocate(n * sizeof(value_type), alignof(value_type));
                if (raw_bytes == nullptr) [[unlikely]] {
                    throw std::bad_alloc{};
                }
                return static_cast<value_type*>(raw_bytes);
            }
        }

        static constexpr void deallocate([[maybe_unused]] const value_type* p,
                                         [[maybe_unused]] const size_type n) noexcept { }

        /**
         * @brief Returns a non-const pointer to the underlying arena for rebinding purpose.
         */
        [[nodiscard]] constexpr arena_type* get_arena() const noexcept {
            return ptr_arena_;
        }

        [[nodiscard]] friend constexpr bool operator==(const arena_view& lhs,
                                                       const arena_view& rhs) noexcept {
            return lhs.ptr_arena_ == rhs.ptr_arena_;
        }
    };

    //////////////////////////////////////////////
    ///      urlicht::pmr::arena_resource      ///
    //////////////////////////////////////////////
    namespace pmr {
        /**
         * @brief Owning wrapper class for urlicht::arena that conforms to the std::pmr::memory_resource interface.
         * @tparam Arena The underlying urlicht::arena for raw memory allocation. Defaults to
         *               urlicht::arena<true, std::allocator<std::byte>, ArenaGrowthPolicy{}>.
         */
        template <typename Arena = arena<>, bool UnsafeAllocInit = false>
        class arena_resource;
    }

    namespace memory_detail {
        template <typename T>
        struct is_arena_resource : std::false_type {};

        template <typename A, bool U>
        struct is_arena_resource<pmr::arena_resource<A, U>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_pmr_arena_resource_v = memory_detail::is_arena_resource<T>::value;

    namespace pmr {

        template <typename Arena, bool UnsafeAllocInit>
        class arena_resource final : public std::pmr::memory_resource,
                                     private memory_detail::zero_overhead_arena<Arena, UnsafeAllocInit> {
            static_assert(is_urlicht_arena_v<Arena>, "Arena should be an instantiation of urlicht::arena");
            using base_arena = memory_detail::zero_overhead_arena<Arena, UnsafeAllocInit>;
        public:
            /**
             * @brief Perfect forwarding constructor delegating to the constructor of Arena. See
             *        arena.h for more details.
             */
            template <typename ...Args>
            requires std::constructible_from<base_arena, Args&&...> &&
                     (!is_urlicht_pmr_arena_resource_v<std::remove_cvref_t<Args>> && ...)
            constexpr arena_resource(Args&&... args)
            noexcept(std::is_nothrow_constructible_v<base_arena, Args&&...>)
            : base_arena(std::forward<Args>(args)...) {}

            constexpr arena_resource(const arena_resource&) = delete;

            constexpr arena_resource(arena_resource&&)
            noexcept(std::is_nothrow_move_constructible_v<base_arena>) = default;

            constexpr arena_resource& operator=(const arena_resource&) = delete;

            constexpr arena_resource& operator=(arena_resource&&)
            noexcept(std::is_nothrow_move_assignable_v<base_arena>) = default;

            ~arena_resource() override { }

            constexpr const base_arena& arena() const noexcept {
                return static_cast<const base_arena&>(*this);
            }

            constexpr void reset() noexcept { base_arena::reset(); }
            constexpr void release() noexcept { base_arena::release(); }

        protected:
            /**
             * @throws std::bad_alloc - if allocation fails.
             * @throws std::bad_array_new_length - if {bytes} plus alignment padding (if any) would
             *         overflow size_t.
             */
            void* do_allocate(size_t bytes, size_t alignment)
            noexcept(UnsafeAllocInit) override {
                if constexpr (UnsafeAllocInit) {
                    return base_arena::unchecked_allocate_initial(bytes, alignment);
                } else {
                    auto* ptr = base_arena::allocate(bytes, alignment);
                    if (ptr == nullptr) [[unlikely]] {
                        throw std::bad_alloc{};
                    }
                    return ptr;
                }
            }

            /**
             * @brief No-op deallocate call-back for compatibility.
             */
            void do_deallocate(void*, size_t, size_t) noexcept override { }

            bool do_is_equal(const memory_resource& other) const noexcept override {
                return this == std::addressof(other);
            }
        };
    }
}



#endif //URLICHT_ARENA_H
