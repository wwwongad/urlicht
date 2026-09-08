#ifndef URLICHT_RESOURCE_VIEW_H
#define URLICHT_RESOURCE_VIEW_H

#include <urlicht/memory/detail/resources_fwd.h>
#include <urlicht/memory/detail/resource_traits.h>
#include <urlicht/internal/config.h>
#include <cstddef>
#include <limits>
#include <new>
#include <type_traits>

namespace urlicht::memory {
    /**
     * @brief A non-owning view to a memory resource instance (e.g. urlicht::memory::arena or
     *        urlicht::memory::concurrent_arena), designed to enable the sharing of the same resource
     *        among multiple objects. It is compatible with std::allocator_traits and hence all standard
     *        containers.
     *
     *        The resource is accepted structurally: any type exposing the allocation entry points used
     *        below (a checked `allocate(bytes, align)` returning an allocation_result, and when
     *        `Opt.unchecked_allocate == true` an `unchecked_allocate(bytes, align)`) can back the view.
     *        When the resource opts into a static no-op deallocate (see detail::has_noop_deallocate),
     *        deallocation through the view is elided entirely.
     *
     * @tparam T The type to be allocated.
     * @tparam Resource The backing memory resource.
     * @tparam Opt Compile-time allocator options. When Opt.unchecked_allocate == true, allocation goes
     *         through the resource's unchecked, no-throw fast path and requires the resource to expose it.
     * @note Ensure that the underlying resource outlives all resource_views pointing to it.
     */
    template <urlicht::concepts::object T, typename Resource, allocator_options Opt>
    class resource_view {
        static_assert(detail::memory_resource<Resource>, "Resource must be a valid memory resource type.");
        static_assert(!Opt.unchecked_allocate || detail::has_unchecked_allocate<Resource>,
            "Opt.unchecked_allocate == true requires the resource to expose an unchecked_allocate fast path");
    public:
        using resource_type = Resource;
    private:
        // Data member
        resource_type* ptr_resource_{};
    public:
        using value_type = T;
        using size_type = size_t;
        using difference_type = std::ptrdiff_t;
        using propagate_on_container_copy_assignment = std::true_type;
        using propagate_on_container_move_assignment = std::true_type;
        using propagate_on_container_swap = std::true_type;
        using allocation_result = detail::allocation_result_impl<value_type*>;

        template <typename U>
        struct rebind {
            using other = resource_view<U, Resource, Opt>;
        };

        /**
         * @brief Returns the allocator_options template parameter.
         */
        [[nodiscard]] static consteval allocator_options options() noexcept {
            return Opt;
        }

        /**
         * @brief Returns the Opt.unchecked_allocate template parameter.
         */
        [[nodiscard]] static consteval bool unchecked_allocate() noexcept {
            return Opt.unchecked_allocate;
        }

        constexpr resource_view() noexcept = delete;

        /**
         * @brief Constructs with a memory resource by holding a reference to it.
         * @param resource A memory resource instance from which resource_view will allocate memory subsequently.
         */
        constexpr resource_view(resource_type& resource) noexcept
        : ptr_resource_(&resource) { }

        constexpr resource_view(const resource_view&) noexcept = default;
        constexpr resource_view(resource_view&&) noexcept = default;

        template <typename U>
        constexpr resource_view(const resource_view<U, Resource, Opt>& other) noexcept
        : ptr_resource_(&other.get_resource()) {
            UL_ASSERT(ptr_resource_ != nullptr, "Pointer to resource must not be null");
        }

        constexpr resource_view& operator=(const resource_view&) noexcept = default;
        constexpr resource_view& operator=(resource_view&&) noexcept = default;

        constexpr ~resource_view() noexcept = default;


        [[nodiscard]] constexpr allocation_result allocate_at_least(const size_type n)
        noexcept(Opt.unchecked_allocate) {
            static_assert(sizeof(value_type) > 0, "Cannot allocate for incomplete types.");
            if constexpr (Opt.unchecked_allocate) {
                auto [ptr, bytes] = ptr_resource_->unchecked_allocate(
                    n * sizeof(value_type), alignof(value_type)
                );
                return {static_cast<value_type*>(ptr), bytes / sizeof(value_type)};
            } else {
                auto bad_size = [](const size_type size) {
                    return size > std::numeric_limits<size_type>::max() / sizeof(value_type);
                };
                if (bad_size(n)) [[unlikely]] {
                    throw std::bad_array_new_length{};
                }
                auto [ptr, bytes] = ptr_resource_->allocate(n * sizeof(value_type), alignof(value_type));
                if (ptr == nullptr) [[unlikely]] {
                    throw std::bad_alloc{};
                }
                return {static_cast<value_type*>(ptr), bytes / sizeof(value_type)};
            }
        }

        [[nodiscard]] constexpr value_type* allocate(const size_type n) noexcept(Opt.unchecked_allocate) {
            return this->allocate_at_least(n).ptr;
        }

        // When the resource's deallocate is a static no-op (e.g. arena/concurrent_arena),
        // deallocation is elided entirely.
        static constexpr void deallocate(value_type*, size_type) noexcept
        requires (detail::has_noop_deallocate<resource_type>::value) { }

        // Otherwise the deallocation is forwarded to the underlying resource (byte size and alignment).
        constexpr void deallocate(value_type* p, const size_type n) noexcept
        requires (!detail::has_noop_deallocate<resource_type>::value) {
            ptr_resource_->deallocate(p, n * sizeof(value_type), alignof(value_type));
        }

        [[nodiscard]] constexpr void* allocate_bytes(const size_type nbytes,
                                                     const size_type align = alignof(std::max_align_t))
        noexcept(Opt.unchecked_allocate) {
            if constexpr (Opt.unchecked_allocate) {
                return ptr_resource_->unchecked_allocate(nbytes, align).ptr;
            } else {
                auto [ptr, _] = ptr_resource_->allocate(nbytes, align);
                if (ptr == nullptr) [[unlikely]] {
                    throw std::bad_alloc{};
                }
                return ptr;
            }
        }

        // Static no-op raw-byte deallocation for resources whose deallocate is a static no-op.
        static constexpr void deallocate_bytes(void*, const size_type,
                                               [[maybe_unused]] const size_type align = alignof(std::max_align_t))
        noexcept requires (detail::has_noop_deallocate<resource_type>::value) { }

        // Raw-byte deallocation forwarded to the underlying resource.
        constexpr void deallocate_bytes(void* p, const size_type nbytes,
                                        const size_type align = alignof(std::max_align_t)) noexcept
        requires (!detail::has_noop_deallocate<resource_type>::value) {
            ptr_resource_->deallocate(p, nbytes, align);
        }

        /**
         * @brief Returns a non-const reference to the underlying resource for rebinding purpose.
         */
        [[nodiscard]] constexpr resource_type& get_resource() const noexcept {
            return *ptr_resource_;
        }

        [[nodiscard]] friend constexpr bool operator==(const resource_view& lhs,
                                                       const resource_view& rhs) noexcept {
            return lhs.ptr_resource_ == rhs.ptr_resource_;
        }
    };
}



#endif //URLICHT_RESOURCE_VIEW_H
