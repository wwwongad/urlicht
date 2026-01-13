#ifndef URLICHT_TAGGED_PTR_H
#define URLICHT_TAGGED_PTR_H
#pragma once
#include <memory>
#include <type_traits>
#include <cstdint>
#include <bit>
#include <utility>
#include <variant>
#include <functional>
#include <concepts>
#include <format>


namespace urlicht {

    template<
        typename T,
        bool IS_OWNING = false,
        typename Deleter = std::default_delete<T>>
    class tagged_ptr {
    public:
        using pointer = std::conditional_t<std::is_array_v<T>, std::decay_t<T>, T*>;
        using element_type = std::conditional_t<std::is_array_v<T>, std::remove_extent_t<T>, T>;
        // Optimizes the deleter away if non owning
        using deleter_type = std::conditional_t<IS_OWNING, Deleter, std::monostate>;
        using tag_size_type = uint8_t;  // size type to hold the number of free bits

        static_assert(std::is_object_v<T>, "T must be an object type");

        static_assert(sizeof(pointer) == sizeof(uintptr_t), "T must be of the same size as uintptr_t"); // For bit_cast

        static_assert(!IS_OWNING || std::invocable<deleter_type, pointer>, "Deleter must be callable with pointer");

        static_assert(alignof(element_type) >= 2, "T must be 2 or more bytes aligned for tag storage");

        static_assert(alignof(element_type) > 0 && !(alignof(element_type) & (alignof(element_type) - 1)),
                     "Alignment must be a power of 2");

    private:
        static constexpr tag_size_type NUM_FREE_BITS = std::countr_zero(alignof(element_type));
        // ptr_ & PTR_MASK == raw pointer
        static constexpr uintptr_t PTR_MASK = ~((static_cast<uintptr_t>(1) << NUM_FREE_BITS) - 1);
        // ptr_ & TAG_MASK == pointer tag
        static constexpr uintptr_t TAG_MASK = ~PTR_MASK;

        uintptr_t ptr_{};
        [[no_unique_address]] deleter_type deleter_{};

    public:
        using tag_type =
            std::conditional_t<NUM_FREE_BITS <= 8,  std::uint8_t,
                std::conditional_t<NUM_FREE_BITS <= 16, std::uint16_t,
                    std::conditional_t<NUM_FREE_BITS <= 32, std::uint32_t, std::uint64_t>
                >
            >;

        /************************* STATIC METHODS *************************/
        [[nodiscard]] static consteval std::size_t num_free_bits() noexcept {
            return NUM_FREE_BITS;
        }

        [[nodiscard]] static consteval bool is_owning() noexcept {
            return IS_OWNING;
        }

        /************************* CONSTRUCTORS *************************/
        constexpr tagged_ptr() noexcept requires std::default_initializable<deleter_type> = default;

        // Value constructor - from a raw pointer
        explicit constexpr tagged_ptr(const pointer ptr) noexcept
        requires std::default_initializable<deleter_type>
        : ptr_{ std::bit_cast<uintptr_t>(ptr) }, deleter_{} {}

        // Value constructor - from a raw pointer and a deleter
        // UB if deleter_type is a rvalue reference but Del is a lvalue reference, or vice versa
        // Disallowed for non owning tagged_ptr
        template <typename Del>
        requires IS_OWNING && std::constructible_from<deleter_type, Del&&>
        constexpr tagged_ptr(const pointer ptr, Del&& del)
        noexcept(std::is_nothrow_constructible_v<deleter_type, Del&&>)
        : ptr_{ std::bit_cast<uintptr_t>(ptr) },  deleter_{std::forward<Del>(del)} {   }

        // Copy constructor - non-owning
        constexpr tagged_ptr(const tagged_ptr& other)
        noexcept(std::is_nothrow_copy_constructible_v<deleter_type>)
        requires (!IS_OWNING) && std::copy_constructible<deleter_type> = default;

        // Move constructor - non-owning
        constexpr tagged_ptr(tagged_ptr&& other)
        noexcept(std::is_nothrow_move_constructible_v<deleter_type>)
        requires (!IS_OWNING) && std::move_constructible<deleter_type> = default;

        // Move constructor - owning
        constexpr tagged_ptr(tagged_ptr&& other)
        noexcept(std::is_nothrow_move_constructible_v<deleter_type>)
        requires IS_OWNING && std::move_constructible<deleter_type>
        : ptr_(other.ptr_), deleter_(std::move(other.deleter_)) {
            other.ptr_ = 0;
        }

        // Copy assignment
        constexpr tagged_ptr& operator=(const tagged_ptr& other)
        noexcept(std::is_nothrow_copy_assignable_v<deleter_type>)
        requires (!IS_OWNING) && std::is_copy_assignable_v<deleter_type> = default;

        // Move assignment - non-owning
        constexpr tagged_ptr& operator=(tagged_ptr&& other)
        noexcept(std::is_nothrow_move_assignable_v<deleter_type>)
        requires (!IS_OWNING) && std::is_move_assignable_v<deleter_type> = default;

        // Move assignment - owning
        constexpr tagged_ptr& operator=(tagged_ptr&& other) noexcept
        requires IS_OWNING && std::is_move_assignable_v<deleter_type> {
            if (this != &other) {
                clear_all();  // Release current resource
                ptr_ = other.ptr_;
                deleter_ = std::move(other.deleter_);
                other.ptr_ = 0;
            }
            return *this;
        }

        constexpr ~tagged_ptr()
        noexcept(!IS_OWNING || std::is_nothrow_invocable_v<deleter_type, pointer>) {
            if constexpr (IS_OWNING) {
                if (get()) {
                    get_deleter()(get());
                }
            }
        }

        /************************* MODIFIERS *************************/

        //////////////////////////////////////////////
        //         Changing the pointer             //
        //////////////////////////////////////////////

        // Releases the pointer, keeps the tag
        [[nodiscard]] constexpr pointer release_ptr() noexcept {
            const pointer tmp = get();
            ptr_ &= TAG_MASK;
            return tmp;
        }

        // Replaces the old pointer and returns it. Keeps current tag.
        [[nodiscard]] constexpr pointer exchange_ptr(pointer new_ptr) noexcept {
            auto tmp = get();
            ptr_ = std::bit_cast<uintptr_t>(new_ptr) | static_cast<uintptr_t>(tag());
            return tmp;
        }

        // Sets new pointer, delete the current one. Keeps current tag.
        constexpr void set_ptr(const pointer new_ptr)
        noexcept(!IS_OWNING || std::is_nothrow_invocable_v<deleter_type, pointer>) {
            auto tmp = get();
            ptr_ = std::bit_cast<uintptr_t>(new_ptr) | static_cast<uintptr_t>(tag());
            if constexpr (IS_OWNING) {
                if (tmp) std::invoke(deleter_, tmp);
            }
        }

        // Assigns raw pointer (keeps current tag). Non-owning only.
        constexpr tagged_ptr& operator= (const pointer other) noexcept
        requires (!IS_OWNING) {
            ptr_ = std::bit_cast<uintptr_t>(other) | static_cast<uintptr_t>(tag());
            return *this;
        }

        // Clears the pointer and leaves the tag unchanged
        constexpr void clear_ptr()
        noexcept(!IS_OWNING || std::is_nothrow_invocable_v<deleter_type, pointer>) {
            const auto tmp = get();
            ptr_ &= TAG_MASK;
            if constexpr (IS_OWNING) {
                if (tmp) std::invoke(deleter_, tmp);
            }
        }

        //////////////////////////////////////////////
        //             Changing the tag             //
        //////////////////////////////////////////////

        constexpr void set_tag(const tag_type tag) noexcept {
            ptr_ = (ptr_ & PTR_MASK) | (static_cast<uintptr_t>(tag) & TAG_MASK);
        }

        // UB if out-of-range
        constexpr void set_tag_bit(const tag_size_type idx) noexcept {
            const uintptr_t bit = uintptr_t{1} << idx;
            ptr_ |= (bit & TAG_MASK);
        }

        constexpr void clear_tag() noexcept {
            ptr_ = ptr_ & PTR_MASK;
        }

        // UB if out-of-range
        constexpr void clear_tag_bit(const tag_size_type idx) noexcept {
            const uintptr_t bit = uintptr_t{1} << idx;
            ptr_ &= ~(bit & TAG_MASK);
        }

        //////////////////////////////////////////////
        //          Clearing and resetting          //
        //////////////////////////////////////////////

        // Releases the pointer and clears the tag
        [[nodiscard]] constexpr pointer release_and_clear() noexcept {
            const auto tmp = get();
            ptr_ = 0;
            return tmp;
        }

        // Deletes the pointer and clears the tag
        constexpr void clear_all()
        noexcept(!IS_OWNING || std::is_nothrow_invocable_v<deleter_type, pointer>) {
            const auto tmp = get();
            ptr_ = 0;
            if constexpr (IS_OWNING) {
                if (tmp) std::invoke(deleter_, tmp);
            }
        }

        // Returns the pointer for non-owning tagged ptr
        // Frees the pointer for owning tagged ptr
        constexpr pointer reset() noexcept
        requires (!IS_OWNING) {
            return release_and_clear();
        }

        constexpr void reset()
        noexcept(std::is_nothrow_invocable_v<deleter_type, pointer>)
        requires (IS_OWNING) {
            const auto tmp = get();
            ptr_ = 0;
            if (tmp) std::invoke(deleter_, tmp);
        }

        constexpr pointer reset(const pointer new_ptr) noexcept
        requires (!IS_OWNING) {
            auto tmp = get();
            ptr_ = std::bit_cast<uintptr_t>(new_ptr);
            return tmp;
        }

        constexpr void reset(const pointer new_ptr)
        noexcept(std::is_nothrow_invocable_v<deleter_type, pointer>)
        requires (IS_OWNING) {
            auto tmp = get();
            ptr_ = std::bit_cast<uintptr_t>(new_ptr);
            if (tmp) std::invoke(deleter_, tmp);
        }

        constexpr pointer reset(const pointer new_ptr, const tag_type tag) noexcept
        requires (!IS_OWNING) {
            auto tmp = get();
            ptr_ = std::bit_cast<uintptr_t>(new_ptr);
            set_tag(tag);
            return tmp;
        }

        constexpr void reset(const pointer new_ptr, tag_type tag)
        noexcept(std::is_nothrow_invocable_v<deleter_type, pointer>)
        requires (IS_OWNING) {
            auto tmp = get();
            ptr_ = std::bit_cast<uintptr_t>(new_ptr);
            set_tag(tag);
            if (tmp) std::invoke(deleter_, tmp);
        }

        /************************* OBSERVERS *************************/

        //////////////////////////////////////////////
        // Retrieving pointer value / array element //
        //////////////////////////////////////////////

        [[nodiscard]] constexpr uintptr_t raw() const noexcept {
            return ptr_;
        }

        [[nodiscard]] constexpr pointer get() const noexcept {
            return std::bit_cast<pointer>(ptr_ & PTR_MASK);
        }

        [[nodiscard]] constexpr pointer ptr() const noexcept {
            return get();
        }

        [[nodiscard]] explicit constexpr operator pointer() const noexcept {
            return get();
        }

        // For tagged_ptr<T>
        [[nodiscard]] constexpr element_type& operator*() const noexcept
        requires (!std::is_array_v<T>) {
            return *get();
        }

        [[nodiscard]] constexpr pointer operator->() const noexcept
        requires (!std::is_array_v<T>) {
            return get();
        }

        // For tagged_ptr<T[]>
        [[nodiscard]] constexpr element_type& operator[](const std::size_t pos) const noexcept
        requires std::is_array_v<T> {
            return get()[pos];
        }

        //////////////////////////////////////////////
        //            Retrieving the tag            //
        //////////////////////////////////////////////

        template <std::integral SizeType = tag_type>
        [[nodiscard]] constexpr SizeType tag() const noexcept {
            return static_cast<SizeType>(ptr_ & TAG_MASK);
        }

        [[nodiscard]] constexpr bool tag_bit(const tag_size_type idx) const noexcept {
            const auto mask = uintptr_t{1} << idx;
            return (ptr_ & mask) != 0;
        }

        //////////////////////////////////////////////
        //          Retrieving the deleter          //
        //////////////////////////////////////////////

        // For owning tagged_ptr only

        [[nodiscard]] constexpr Deleter& get_deleter() noexcept
        requires IS_OWNING {
            return deleter_;
        }

        [[nodiscard]] constexpr const Deleter& get_deleter() const noexcept
        requires IS_OWNING {
            return deleter_;
        }

        /************************* UTILITIES *************************/

        [[nodiscard]] explicit constexpr operator bool() const noexcept {
            return get() != nullptr;
        }

        // When comparing between two tagged_ptr, the tags are included
        // When comparing between a tagger_ptr and a pointer, the tag is not considered

        friend constexpr bool operator==(const tagged_ptr& lhs, const tagged_ptr& rhs) noexcept {
            return lhs.ptr_ == rhs.ptr_;
        }

        friend constexpr bool operator==(const tagged_ptr& lhs, const pointer rhs) noexcept {
            return lhs.get() == rhs;
        }

        friend constexpr bool operator==(const pointer lhs, const tagged_ptr& rhs) noexcept {
            return lhs == rhs.get();
        }

        friend constexpr auto operator<=>(const tagged_ptr& lhs, const tagged_ptr& rhs) noexcept {
            return lhs.ptr_ <=> rhs.ptr_;
        }

        friend constexpr auto operator<=>(const tagged_ptr& lhs, const pointer rhs) noexcept {
            return lhs.get() <=> rhs;
        }

        friend constexpr auto operator<=>(const pointer lhs, const tagged_ptr& rhs) noexcept {
            return lhs <=> rhs.get();
        }

        constexpr void swap(tagged_ptr& other)
        noexcept(std::is_empty_v<deleter_type> || std::is_nothrow_swappable_v<deleter_type>)
        requires std::swappable<deleter_type> {
            using std::swap;
            swap(ptr_, other.ptr_);
            if constexpr(!std::is_empty_v<deleter_type>) {
                swap(deleter_, other.deleter_);
            }
        }

        friend constexpr void swap(tagged_ptr& lhs, tagged_ptr& rhs)
        noexcept(noexcept(lhs.swap(rhs)))
        requires std::swappable<deleter_type> {
            lhs.swap(rhs);
        }

        // prints the underlying uintptr_t
        friend std::ostream& operator<<(std::ostream& os, const tagged_ptr& ptr) {
            return os << ptr.ptr_;
        }

    };

    template <typename T, bool IS_OWNING = true, typename... Args>
    tagged_ptr<T, IS_OWNING> make_tagged(Args&&... args)
    requires (!std::is_array_v<T>) && std::constructible_from<T, Args&&...> {
        return tagged_ptr<T, IS_OWNING>(new T(std::forward<Args>(args)...));
    }

    template <typename T, bool IS_OWNING = true, std::integral SizeType>
    tagged_ptr<T, IS_OWNING> make_tagged(SizeType size)
    requires std::is_array_v<T> && std::default_initializable<std::remove_extent_t<T>> {
        using element_type = std::remove_extent_t<T>;
        return tagged_ptr<T, IS_OWNING>(new element_type[size]{});
    }

    // CTAD
    template <typename P>
    tagged_ptr(P*) -> tagged_ptr<std::remove_pointer_t<P>>;

    template <typename P, typename D>
    tagged_ptr(P*, D) -> tagged_ptr<std::remove_pointer_t<P>, true, D>;


    // Structural binding
    template <std::size_t I, typename T, bool IS_OWNING, typename Deleter>
    constexpr auto get(const tagged_ptr<T, IS_OWNING, Deleter>& p)
    noexcept -> std::conditional_t<I == 0,
                                   typename tagged_ptr<T, IS_OWNING, Deleter>::pointer,
                                   typename tagged_ptr<T, IS_OWNING, Deleter>::tag_type> {
        if constexpr (I == 0) {
            return p.get();
        } else {
            return p.tag();
        }
    }

    template <std::size_t I, typename T, bool IS_OWNING, typename Deleter>
    constexpr auto get(tagged_ptr<T, IS_OWNING, Deleter>& p)
    noexcept -> std::conditional_t<I == 0,
                                   typename tagged_ptr<T, IS_OWNING, Deleter>::pointer,
                                   typename tagged_ptr<T, IS_OWNING, Deleter>::tag_type> {
        if constexpr (I == 0) {
            return p.get();
        } else {
            return p.tag();
        }
    }

    template <std::size_t I, typename T, bool IS_OWNING, typename Deleter>
    constexpr auto get(tagged_ptr<T, IS_OWNING, Deleter>&& p)
    noexcept -> std::conditional_t<I == 0,
                                   typename tagged_ptr<T, IS_OWNING, Deleter>::pointer,
                                   typename tagged_ptr<T, IS_OWNING, Deleter>::tag_type> {
        if constexpr (I == 0) {
            return p.get();
        } else {
            return p.tag();
        }
    }

} // namespace urlicht

namespace std {

    // Structural binding
    // tuple_size
    template <typename T, bool IS_OWNING, typename Deleter>
    struct tuple_size<urlicht::tagged_ptr<T, IS_OWNING, Deleter>>
        : std::integral_constant<std::size_t, 2> {};

    // tuple_element
    template <std::size_t I, typename T, bool IS_OWNING, typename Deleter>
    struct tuple_element<I, urlicht::tagged_ptr<T, IS_OWNING, Deleter>> {
        static_assert(I < 2, "tagged_ptr tuple_element: index out of bounds");
        using type = std::conditional_t<I == 0,
                                        typename urlicht::tagged_ptr<T, IS_OWNING, Deleter>::pointer,
                                        typename urlicht::tagged_ptr<T, IS_OWNING, Deleter>::tag_type>;
    };

    // std::format
    template <typename T, bool IS_OWNING, typename Deleter, typename CharT>
    struct formatter<urlicht::tagged_ptr<T, IS_OWNING, Deleter>, CharT>
      : std::formatter<uintptr_t, CharT> {
        template <typename FormatContext>
        auto format(const urlicht::tagged_ptr<T, IS_OWNING, Deleter>& p,
                    FormatContext& ctx) const {
            return std::formatter<uintptr_t, CharT>::format(p.raw(), ctx);
        }
    };

    namespace detail_pointer_traits {

        template <typename D>
        struct is_default_delete : std::false_type {};

        template <typename X>
        struct is_default_delete<std::default_delete<X>> : std::true_type {};

        template <typename Del>
        inline constexpr bool is_default_delete_v = is_default_delete<Del>::value;

    }


    template <typename T, bool IS_OWNING, typename Deleter>
    struct pointer_traits<urlicht::tagged_ptr<T, IS_OWNING, Deleter>> {
        using pointer = urlicht::tagged_ptr<T, IS_OWNING, Deleter>;
        using element_type = typename pointer::element_type; // May not be T, depending on whether it's an array
        using difference_type = std::ptrdiff_t;

        //  1. std::default_delete<U> if Deleter is std::default_delete<...>
        //  2. otherwise keeps Deleter unchanged (best-effort fallback)
        template <typename U>
        using rebind =
            urlicht::tagged_ptr<
                U,
                IS_OWNING,
                std::conditional_t<
                    detail_pointer_traits::is_default_delete_v<Deleter>,
                    std::default_delete<U>,
                    Deleter
                >
            >;

        constexpr static pointer pointer_to(element_type& r) noexcept
        requires std::default_initializable<typename pointer::deleter_type> { // Since Deleter can't be provided here
            return pointer(std::addressof(r));
        }

        constexpr static element_type* to_address(const pointer& p) noexcept {
            return p.get();
        }
    };

    // std::hash
    template<typename T, bool IS_OWNING, typename Deleter>
    struct hash<urlicht::tagged_ptr<T, IS_OWNING, Deleter>> {
        std::size_t operator()(const urlicht::tagged_ptr<T, IS_OWNING, Deleter>& p) noexcept {
            return std::hash<uintptr_t>{}(p.raw()); // both pointer and tag
        }
    };

} // namespace std

    #endif //URLICHT_TAGGED_PTR_H
