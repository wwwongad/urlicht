#ifndef URLICHT_MEMORY_PMR_ARENA_RESOURCE_H
#define URLICHT_MEMORY_PMR_ARENA_RESOURCE_H

#include <urlicht/memory/detail/arena_fwd.h>
#include <urlicht/memory/detail/resource_traits.h>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/concurrent_arena.h>
#include <memory_resource>
#include <concepts>
#include <type_traits>
#include <cstddef>

namespace urlicht::memory::pmr {

    /**
     * @brief An owning wrapper for urlicht::memory::arena (or urlicht::memory::concurrent_arena)
     *        that conforms to the std::pmr::memory_resource interface. It inherits the underlying
     *        arena's constructors and adds no data members of its own (zero-overhead wrapping via
     *        private inheritance).
     *
     *        do_allocate forwards to the arena; do_deallocate is a no-op (per-object frees are not
     *        reclaimed - use reset()/release()); do_is_equal is identity comparison, so each resource
     *        serves only containers that hold this exact object.
     *
     * @tparam Arena The underlying arena type.
     * @tparam Opt Compile-time allocator options. When Opt.unchecked_allocate == true, do_allocate uses
     *         the arena's unchecked, no-throw fast path (unchecked_allocate) and is noexcept; the arena
     *         must expose that fast path.
     */
    template <typename Arena, allocator_options Opt>
    class arena_resource final : public std::pmr::memory_resource,
                                 private Arena {
        static_assert(detail::has_noop_deallocate<Arena>,
            "Arena must expose a static no-op deallocate");
        static_assert(!Opt.unchecked_allocate || detail::has_unchecked_allocate<Arena>,
            "Opt.unchecked_allocate == true requires the arena to expose an unchecked_allocate fast path");
    public:
        using Arena::Arena;

        arena_resource(const arena_resource&) = delete;
        arena_resource& operator=(const arena_resource&) = delete;

        constexpr arena_resource(arena_resource&& other)
        noexcept(std::is_nothrow_move_constructible_v<Arena>)
        : std::pmr::memory_resource{}, Arena{std::move(static_cast<Arena&>(other))}
        { }

        constexpr arena_resource& operator=(arena_resource&& other)
        noexcept(std::is_nothrow_move_assignable_v<Arena>) {
            if (this != &other) [[likely]] {
                static_cast<Arena&>(*this) = std::move(static_cast<Arena&>(other));
            }
            return *this;
        }

        constexpr ~arena_resource() override = default;

        constexpr const Arena& arena() const noexcept {
            return static_cast<const Arena&>(*this);
        }

        constexpr void reset() noexcept { Arena::reset(); }
        constexpr void release() noexcept { Arena::release(); }

    protected:
        /**
         * @throws std::bad_alloc - if allocation fails.
         * @throws std::bad_array_new_length - if {bytes} plus alignment padding (if any) would
         *         overflow size_t.
         */
        void* do_allocate(size_t bytes, size_t alignment)
        noexcept(Opt.unchecked_allocate) override {
            if constexpr (Opt.unchecked_allocate) {
                return Arena::unchecked_allocate(bytes, alignment).ptr;
            } else {
                auto [ptr, cnt] = Arena::allocate(bytes, alignment);
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

#endif //URLICHT_MEMORY_PMR_ARENA_RESOURCE_H
