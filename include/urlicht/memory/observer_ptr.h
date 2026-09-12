#ifndef URLICHT_MEMORY_OBSERVER_PTR_H
#define URLICHT_MEMORY_OBSERVER_PTR_H
#include <urlicht/internal/config.h>

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <iosfwd>
#include <memory>
#include <type_traits>
#include <compare>
#include <utility>

namespace urlicht::memory {

    /**
     * @brief A non-owning smart pointer observing an object managed elsewhere. observer_ptr is designed to
     *        avoid problems associated with the mixed usage of smart pointers and raw pointers, notably
     *        accidentally deleting a pointer to resources owned by smart pointers. It acts as a
     *        substitute of raw pointers except that it cannot be deleted (compilation error). With the
     *        correct use of observer_ptr, it is expected that raw lvalue pointers should never appear in
     *        application program.
     *
     * @tparam T The type of the object to be observed.
     */
    template <typename T>
    class observer_ptr {
        static_assert(std::is_object_v<T>, "T must be an object type");

        template <typename> friend class observer_ptr;
    public:
        using element_type = T;
        using pointer = element_type*;
        using const_pointer = const element_type*;
        using reference = std::add_lvalue_reference_t<element_type>;

        /************************* CONSTRUCTORS *************************/

        /**
         * @brief Constructs an empty observer that observes nothing.
         */
        constexpr observer_ptr() noexcept = default;

        /**
         * @brief Constructs an empty observer that observes nothing.
         */
        constexpr observer_ptr(std::nullptr_t) noexcept {}

        /**
         * @brief Implicitly constructs an observer of the object pointed to by ptr.
         */
        constexpr observer_ptr(const pointer ptr) noexcept
        : ptr_{ptr} {}

        /**
         * @brief Implicitly constructs an observer of the lvalue object {obj}.
         * @note Only lvalues bind. Observing a rvalue would leave the observer dangling as soon as
         *       the temporary dies at the end of the full expression, so the rvalue overload is deleted.
         */
        constexpr observer_ptr(reference obj) noexcept
        : ptr_{std::addressof(obj)} {}

        constexpr observer_ptr(element_type&&) noexcept = delete;

        /**
         * @brief Observes the object owned by {up}. The unique_ptr must outlive the observer.
         * @note Rvalues are rejected: a temporary owner would destroy the observed object at the end
         *       of the full expression, leaving the observer dangling. The pointee type U is deducible
         *       so a derived owner (e.g. unique_ptr<Derived> -> observer_ptr<Base>) is accepted.
         */
        template <typename U, typename Deleter>
        requires (std::convertible_to<typename std::unique_ptr<U, Deleter>::pointer, pointer>)
        constexpr observer_ptr(const std::unique_ptr<U, Deleter>& up) noexcept
        : ptr_{up.get()} { }

        template <typename U, typename Deleter>
        constexpr observer_ptr(std::unique_ptr<U, Deleter>&&) noexcept = delete;

        /**
         * @brief Observes the object owned by {sp}.
         * @note Rvalues are rejected for the same reason as unique_ptr above.
         */
        template <typename U>
        requires (std::convertible_to<typename std::shared_ptr<U>::element_type*, pointer>)
        constexpr observer_ptr(const std::shared_ptr<U>& sp) noexcept
        : ptr_{sp.get()} { }

        template <typename U>
        constexpr observer_ptr(std::shared_ptr<U>&&) noexcept = delete;

        /**
         * @brief Converting constructor: observes the object observed by @p{other}.
         * @note U* must be convertible to T*, so observers of derived objects convert to
         *       observers of base objects, mirroring raw-pointer convertibility.
         */
        template <typename U>
        requires (!std::same_as<U, T>) && std::convertible_to<typename observer_ptr<U>::pointer, pointer>
        constexpr observer_ptr(const observer_ptr<U>& other) noexcept
        : ptr_{other.ptr_} {}

        constexpr observer_ptr(const observer_ptr&) noexcept = default;
        constexpr observer_ptr(observer_ptr&&) noexcept = default;

        constexpr observer_ptr& operator=(const observer_ptr&) noexcept = default;
        constexpr observer_ptr& operator=(observer_ptr&&) noexcept = default;

        constexpr ~observer_ptr() = default;

        /************************* MODIFIERS *************************/


        constexpr observer_ptr& operator=(const pointer ptr) noexcept {
            ptr_ = ptr;
            return *this;
        }

        constexpr observer_ptr& operator=(std::nullptr_t) noexcept {
            ptr_ = nullptr;
            return *this;
        }

        constexpr void reset(const pointer ptr = nullptr) noexcept {
            ptr_ = ptr;
        }

        constexpr void clear() noexcept {
            ptr_ = nullptr;
        }

        constexpr void swap(observer_ptr& other) noexcept {
            using std::swap;
            swap(ptr_, other.ptr_);
        }

        friend constexpr void swap(observer_ptr& lhs, observer_ptr& rhs) noexcept {
            lhs.swap(rhs);
        }

        /************************* OBSERVERS *************************/


        /**
         * @brief Dereferences the observed object.
         * @warning Observing nothing is a precondition violation; asserted in debug builds.
         */
        [[nodiscard]] constexpr reference operator*() const noexcept {
            UL_ASSERT(ptr_ != nullptr, "Dereferencing an empty observer_ptr");
            return *ptr_;
        }

        /**
         * @brief Member access to the observed object.
         */
        [[nodiscard]] constexpr pointer operator->() const noexcept {
            return ptr_;
        }

        /**
         * @brief Returns whether the observer observes an object.
         */
        [[nodiscard]] constexpr explicit operator bool() const noexcept {
            return ptr_ != nullptr;
        }

        /**
         * @brief Returns the underlying pointer.
         * @warning Use this in a rvalue context only, e.g. passing arguments to a function taking raw pointers.
         *          Do NOT retain it.
         */
        [[nodiscard]] constexpr pointer get() const noexcept {
            return ptr_;
        }

        /**
         * @brief The observed address as an integer, for hashing and formatting.
         */
        [[nodiscard]] constexpr std::uintptr_t raw() const noexcept {
            return reinterpret_cast<std::uintptr_t>(ptr_);
        }

        /**
         * @brief Explicitly converts to a raw pointer.
         * @warning Use this in a rvalue context only. Do not retain it.
         */
        [[nodiscard]] explicit constexpr operator pointer() const noexcept {
            return ptr_;
        }

        //************************ Comparisons ************************//
        // Compares the observed addresses.

        [[nodiscard]] friend constexpr bool operator==(const observer_ptr& lhs,
                                                       const observer_ptr& rhs) noexcept {
            return lhs.ptr_ == rhs.ptr_;
        }

        [[nodiscard]] friend constexpr bool operator==(const observer_ptr& lhs,
                                                       const pointer rhs) noexcept {
            return lhs.ptr_ == rhs;
        }

        [[nodiscard]] friend constexpr bool operator==(const pointer lhs,
                                                       const observer_ptr& rhs) noexcept {
            return lhs == rhs.ptr_;
        }

        [[nodiscard]] friend constexpr auto operator<=>(const observer_ptr& lhs,
                                                        const observer_ptr& rhs) noexcept {
            return lhs.ptr_ <=> rhs.ptr_;
        }

        [[nodiscard]] friend constexpr auto operator<=>(const observer_ptr& lhs,
                                                        const pointer rhs) noexcept {
            return lhs.ptr_ <=> rhs;
        }

        [[nodiscard]] friend constexpr auto operator<=>(const pointer lhs,
                                                        const observer_ptr& rhs) noexcept {
            return lhs <=> rhs.ptr_;
        }

        // Prints the underlying uintptr_t
        friend std::ostream& operator<<(std::ostream& os, const observer_ptr& ptr) {
            return os << ptr.raw();
        }

    private:
        pointer ptr_{nullptr};
    };

    // CTAD guides
    template <typename P>
    observer_ptr(P*) -> observer_ptr<P>;

    // Constrained so observer_ptr{&x} deduces observer_ptr<int> through the pointer guide
    // instead of observer_ptr<int*> through this one.
    template <typename U>
    requires (!std::is_pointer_v<U>)
    observer_ptr(U&) -> observer_ptr<U>;

    namespace detail {
        template <typename>
        struct is_observer_ptr : std::false_type {};
        template <typename T>
        struct is_observer_ptr<observer_ptr<T>> : std::true_type {};
    }
}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_observer_ptr_v = memory::detail::is_observer_ptr<T>::value;
}

namespace std {
    // std::pointer_traits
    template <typename T>
    struct pointer_traits<urlicht::memory::observer_ptr<T>> {
        using pointer = urlicht::memory::observer_ptr<T>;
        using element_type = typename pointer::element_type;
        using difference_type = std::ptrdiff_t;

        template <typename U>
        using rebind = urlicht::memory::observer_ptr<U>;

        constexpr static pointer pointer_to(element_type& r) noexcept {
            return pointer(std::addressof(r));
        }

        constexpr static element_type* to_address(const pointer& p) noexcept {
            return p.get();
        }
    };

    // std::hash
    template <typename T>
    struct hash<urlicht::memory::observer_ptr<T>> {
        [[nodiscard]] std::size_t operator()(const urlicht::memory::observer_ptr<T>& p) const noexcept {
            return std::hash<typename urlicht::memory::observer_ptr<T>::pointer>{}(p.get());
        }
    };

    // std::format
    template <typename T, typename CharT>
    struct formatter<urlicht::memory::observer_ptr<T>, CharT>
      : std::formatter<std::uintptr_t, CharT> {
        template <typename FormatContext>
        [[nodiscard]] auto format(const urlicht::memory::observer_ptr<T>& p, FormatContext& ctx) const {
            return std::formatter<std::uintptr_t, CharT>::format(p.raw(), ctx);
        }
    };
}

#endif //URLICHT_MEMORY_OBSERVER_PTR_H
