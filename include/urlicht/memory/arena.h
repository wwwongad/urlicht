#ifndef URLICHT_ARENA_H
#define URLICHT_ARENA_H

#include <urlicht/config.h>
#include <urlicht/concepts_utility.h>
#include <urlicht/memory/detail/arena_fwd.h>
#include <memory>
#include <cstdint>
#include <cstddef>
#include <limits>
#include <algorithm>
#include <concepts>

namespace urlicht {

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
     * @note: - This class is typeless and thus incompatible with std::allocator_traits. Use urlicht::arena_view
     *          instead for STL-compatibility.
     * @note: - This class is not thread-safe.
     * @note: - If providing an external buffer to the class as the initial buffer, it will not be freed in
     *          release()/destructor, and it is the user's responsibility to manage it.
     */
    template <bool UseUpstream, /* = true */
              ArenaGrowthPolicy GrowthPolicy /* = {} */,
              concepts::allocator UpstreamAlloc /* = std::allocator<std::byte> */>
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
        static constexpr void deallocate(void*, size_t, [[maybe_unused]] size_t align = default_align) noexcept { }

        friend constexpr bool operator==(const arena& lhs, const arena& rhs) noexcept {
            return std::addressof(lhs) == std::addressof(rhs);
        }

    };

}



#endif //URLICHT_ARENA_H
