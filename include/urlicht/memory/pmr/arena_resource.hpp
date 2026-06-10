#ifndef URLICHT_ARENA_RESOURCE_H
#define URLICHT_ARENA_RESOURCE_H

#include <urlicht/memory/detail/arena_fwd.h>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/concurrent_arena.h>
#include <memory_resource>
#include <concepts>
#include <type_traits>
#include <cstddef>

namespace urlicht::pmr {

    template <typename Arena, bool UnsafeAllocInit>
    class arena_resource final : public std::pmr::memory_resource,
                                 private memory_detail::zero_overhead_arena<Arena, UnsafeAllocInit> {
        static_assert(is_urlicht_arena_view_compatible_v<Arena>,
            "Arena should be an instantiation of urlicht::arena or urlicht::concurrent_arena");
        using base_arena = memory_detail::zero_overhead_arena<Arena, UnsafeAllocInit>;
    public:
        using base_arena::base_arena;

        arena_resource(const arena_resource&) = delete;
        arena_resource& operator=(const arena_resource&) = delete;

        constexpr arena_resource(arena_resource&& other)
        noexcept(std::is_nothrow_move_constructible_v<base_arena>)
        : std::pmr::memory_resource{}, base_arena{std::move(static_cast<base_arena&>(other))}
        { }

        constexpr arena_resource& operator=(arena_resource&& other)
        noexcept(std::is_nothrow_move_assignable_v<base_arena>) {
            if (this != &other) [[likely]] {
                static_cast<base_arena&>(*this) = std::move(static_cast<base_arena&>(other));
            }
            return *this;
        }

        constexpr ~arena_resource() override = default;

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
                return base_arena::unchecked_allocate_initial(bytes, alignment).ptr;
            } else {
                auto [ptr, cnt] = base_arena::allocate(bytes, alignment);
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

#endif //URLICHT_ARENA_RESOURCE_H
