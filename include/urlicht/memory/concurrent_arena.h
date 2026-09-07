#ifndef URLICHT_MEMORY_CONCURRENT_ARENA_H
#define URLICHT_MEMORY_CONCURRENT_ARENA_H

#include <urlicht/memory/detail/arena_fwd.h>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/allocation_options.h>
#include <mutex>
#include <concepts>
#include <cstddef>

namespace urlicht::memory {

    /**
     * @brief A thread-safe wrapper for urlicht::memory::arena that uses std::mutex for synchronization.
     *        It preserves all methods provided by urlicht::memory::arena. See arena.h for detail.
     */
    template <resource_options Opt,
              arena_growth_policy GrowthPolicy,
              urlicht::concepts::allocator UpstreamAlloc>
    class concurrent_arena : private arena<Opt, GrowthPolicy, UpstreamAlloc> {
        static_assert(std::same_as<typename UpstreamAlloc::value_type, std::byte>,
            "Upstream allocator must allocate std::byte");

    public:
        using upstream_allocator = UpstreamAlloc;
        using upstream_traits = std::allocator_traits<upstream_allocator>;
        using arena_type = arena<Opt, GrowthPolicy, UpstreamAlloc>;
        using arena_type::arena_type;
        using allocation_result = typename arena_type::allocation_result;

        static constexpr std::size_t default_align = alignof(std::max_align_t);

        concurrent_arena(const concurrent_arena&) = delete;
        concurrent_arena& operator=(const concurrent_arena&) = delete;
        concurrent_arena(concurrent_arena&&) = delete;
        concurrent_arena& operator=(concurrent_arena&&) = delete;

        constexpr ~concurrent_arena() = default;

        /**
         * @brief Returns a const reference to the underlying arena.
         */
        [[nodiscard]] constexpr const arena_type& get_arena() const noexcept {
            return static_cast<const arena_type&>(*this);
        }

        //************************* CORE METHODS ****************************//

        UL_CONSTEXPR23 void release() noexcept {
            std::scoped_lock lock(mutex_);
            get_arena_mut_().release();
        }

        UL_CONSTEXPR23 void reset() noexcept {
            std::scoped_lock lock(mutex_);
            get_arena_mut_().reset();
        }

        /**
         * @brief Perform an unconditional allocation from the initial buffer under the lock.
         * @note UB if the initial buffer is null or undersized.
         */
        [[nodiscard]] UL_CONSTEXPR23 allocation_result unchecked_allocate(const size_t bytes,
                                                                          const size_t align = default_align) noexcept {
            std::scoped_lock lock(mutex_);
            return get_arena_mut_().unchecked_allocate(bytes, align);
        }

        [[nodiscard]] UL_CONSTEXPR23 allocation_result allocate(const size_t bytes,
                                                                const size_t align = default_align) {
            std::scoped_lock lock(mutex_);
            return get_arena_mut_().allocate(bytes, align);
        }

        static constexpr void deallocate(void*, size_t, [[maybe_unused]] size_t align = default_align) noexcept
        { }

        friend constexpr bool operator==(const concurrent_arena& lhs, const concurrent_arena& rhs) noexcept {
            return std::addressof(lhs) == std::addressof(rhs);
        }
    private:
        [[nodiscard]] constexpr arena_type& get_arena_mut_() noexcept {
            return static_cast<arena_type&>(*this);
        }
        // Data member
        std::mutex mutex_;
    };
}

#endif //URLICHT_MEMORY_CONCURRENT_ARENA_H
