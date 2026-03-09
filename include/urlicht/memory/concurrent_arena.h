#ifndef URLICHT_CONCURRENT_ARENA_H
#define URLICHT_CONCURRENT_ARENA_H

#include <urlicht/memory/detail/arena_fwd.h>
#include <urlicht/memory/arena.h>
#include <mutex>
#include <concepts>
#include <type_traits>
#include <cstddef>

namespace urlicht {

    /**
     * @brief A thread-safe wrapper for urlicht::arena that uses std::mutex for synchronization.
     *        It preserves all methods provided by urlicht::arena. See arena.h for detail.
     */
    template <bool UseUpstream,
              ArenaGrowthPolicy GrowthPolicy,
              concepts::allocator UpstreamAlloc>
    class concurrent_arena {
        static_assert(std::same_as<typename UpstreamAlloc::value_type, std::byte>,
            "Upstream allocator must allocate std::byte");
    public:
        using upstream_allocator = UpstreamAlloc;
        using upstream_traits = std::allocator_traits<upstream_allocator>;
        using arena_type = arena<UseUpstream, GrowthPolicy, UpstreamAlloc>;

        static constexpr size_t default_align = alignof(std::max_align_t);
    private:
        std::mutex mutex_;
        arena_type arena_;
    public:
        static consteval bool use_upstream() noexcept {
            return UseUpstream;
        }

        static consteval ArenaGrowthPolicy growth_policy() noexcept requires(UseUpstream) {
            return GrowthPolicy;
        }

        /************************* CONSTRUCTORS ****************************/
        /**
         * @brief Constructor delegating entirely to the constructors of urlicht::arena.
         */
        template <typename... Args>
        requires std::constructible_from<arena_type, Args&&...>
             && (!is_urlicht_arena_v<std::remove_cvref_t<Args>> && ...)
             && (!is_urlicht_concurrent_arena_v<std::remove_cvref_t<Args>> && ...)
        constexpr concurrent_arena(Args&& ...args)
        noexcept(std::is_nothrow_constructible_v<arena_type, Args&&...>)
        : arena_{std::forward<Args>(args)...} { }

        constexpr concurrent_arena(const arena_type&) = delete;
        constexpr concurrent_arena& operator=(const arena_type&) = delete;

        constexpr concurrent_arena(arena_type&& arena)
        noexcept(std::is_nothrow_move_constructible_v<arena_type>)
        : arena_{std::move(arena)} { }

        constexpr concurrent_arena& operator=(arena_type&& arena)
        noexcept(std::is_nothrow_move_assignable_v<arena_type>) {
            if (&arena_ != &arena) {
                arena_ = std::move(arena);
            }
            return *this;
        }

        constexpr concurrent_arena(const concurrent_arena&) = delete;
        constexpr concurrent_arena& operator=(const concurrent_arena&) = delete;

        constexpr concurrent_arena(concurrent_arena&& concurrent_arena)
        noexcept(std::is_nothrow_move_constructible_v<arena_type>)
        : arena_{std::move(concurrent_arena.arena_)} { }

        constexpr concurrent_arena& operator=(concurrent_arena&& other)
        noexcept(std::is_nothrow_move_assignable_v<arena_type>) {
            if (this != &other) {
                arena_ = std::move(other.arena_);
            }
            return *this;
        }

        constexpr ~concurrent_arena() noexcept = default;

        /**
         * @brief Returns a const reference to the underlying arena.
         */
        [[nodiscard]] constexpr const arena_type& get_arena() const noexcept {
            return arena_;
        }

        /************************* CORE METHODS ****************************/

        UL_CONSTEXPR23 void release() noexcept {
            std::unique_lock lock(mutex_);
            arena_.release();
        }

        UL_CONSTEXPR23 void reset() noexcept {
            std::unique_lock lock(mutex_);
            arena_.reset();
        }

        [[nodiscard]] UL_CONSTEXPR23 void* unchecked_allocate_initial(const size_t bytes,
                                                                      const size_t align = default_align) noexcept {
            std::unique_lock lock(mutex_);
            return arena_.unchecked_allocate_initial(bytes, align);
        }

        [[nodiscard]] UL_CONSTEXPR23 void* allocate(const size_t bytes, const size_t align = default_align) {
            std::unique_lock lock(mutex_);
            return arena_.allocate(bytes, align);
        }

        static constexpr void deallocate(void*, size_t, [[maybe_unused]] size_t align = default_align) noexcept
        { }

        friend constexpr bool operator==(const concurrent_arena& lhs, const concurrent_arena& rhs) noexcept {
            return std::addressof(lhs) == std::addressof(rhs);
        }
    };
}

#endif //URLICHT_CONCURRENT_ARENA_H
