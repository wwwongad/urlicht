
#ifndef URLICHT_ARENA_VIEW_H
#define URLICHT_ARENA_VIEW_H
#include <urlicht/memory/detail/arena_fwd.h>
#include <limits>
#include <cstddef>
#include <urlicht/memory/arena.h>
#include <urlicht/memory/concurrent_arena.h>
#include <type_traits>


namespace urlicht {
    /**
     * @brief A non-owning view to an allocator instance (either of type urlicht::arena or urlicht::concurrent_arena),
     *        designed to enable the sharing of the same allocator among multiple objects. It is compatible with
     *        std::allocator_traits and hence all standard containers.
     * @tparam T The type to be allocated.
     * @note Ensure that the underlying arena outlives all arena_views pointing to it.
     */
    template <concepts::object T, bool UnsafeAllocInit, typename Arena>
    class arena_view {
        static_assert(is_urlicht_arena_view_compatible_v<Arena>,
            "Arena should be an instantiation of urlicht::arena or urlicht::concurrent_arena");
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

        template <concepts::object U>
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
}



#endif //URLICHT_ARENA_VIEW_H
