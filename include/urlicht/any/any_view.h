#ifndef URLICHT_ANY_VIEW_H
#define URLICHT_ANY_VIEW_H

#include <typeinfo>
#include <type_traits>
#include <utility>
#include <any>
#include <iostream>
#include <urlicht/concepts_utility.h>

namespace urlicht {
    /**
     * @brief A light weight, non-owning view to any type
     */
    class any_view;

    namespace any_detail {
        template <typename T>
        struct is_any_view { static constexpr bool value = false; };

        template <>
        struct is_any_view<any_view> { static constexpr bool value = true; };
    }

    template <typename T>
    inline constexpr bool is_urlicht_any_view = any_detail::is_any_view<T>::value;

    class any_view {
    private:
        struct meta {
            const std::type_info* (*type_info)() noexcept;
        };

        template <typename T>
        static constexpr meta meta_for = {
            .type_info = []() noexcept { return &typeid(T); }
        };

        // Data members
        const void* ptr_{nullptr};
        const meta* meta_{nullptr};

    public:
        /****************** CONSTRUCTORS *******************/

        constexpr any_view() noexcept = default;

        template <typename T>
        requires (!is_urlicht_any_view<std::remove_cvref_t<T>>
                && !std::is_pointer_v<std::decay_t<T>>
                && !std::same_as<std::decay_t<T>, std::nullptr_t>)
        constexpr any_view(T&& val) noexcept
        : ptr_{std::addressof(val)}, meta_{&meta_for<std::remove_cvref_t<T>>} {
        }

        template <typename T>
        constexpr any_view(const T* p) noexcept
        : ptr_{p}, meta_{p ? &meta_for<std::remove_cvref_t<T>> : nullptr} {
        }

        constexpr any_view(std::nullptr_t) noexcept {} // Initialize both fields as nullptr

        constexpr any_view(const any_view&) noexcept = default;
        constexpr any_view(any_view&&) noexcept = default;
        constexpr any_view& operator=(const any_view&) noexcept = default;
        constexpr any_view& operator=(any_view&&) noexcept = default;

        constexpr ~any_view() noexcept = default;

        /******************** MODIFIERS *********************/

        template <typename T>
        requires (!std::is_pointer_v<std::decay_t<T>>
               && !std::same_as<std::decay_t<T>, std::nullptr_t>)
        constexpr void view(T&& val) noexcept {
            ptr_ = std::addressof(val);
            meta_ = &meta_for<std::remove_cvref_t<T>>;
        }

        template <typename T>
        constexpr void view(const T* ptr) noexcept {
            if (ptr) [[likely]] {
                ptr_ = ptr;
                meta_ = &meta_for<std::remove_cv_t<T>>;
            } else {
                reset();
            }
        }

        constexpr void view(std::nullptr_t) noexcept {
            reset();
        }

        constexpr void reset() noexcept {
            ptr_ = nullptr;
            meta_ = nullptr;
        }

        constexpr void swap(any_view& other) noexcept {
            std::swap(ptr_, other.ptr_);
            std::swap(meta_, other.meta_);
        }

        /******************** OBSERVERS *********************/

        [[nodiscard]] constexpr bool has_value() const noexcept {
            return ptr_ != nullptr;
        }

        [[nodiscard]] constexpr explicit operator bool() const noexcept {
            return has_value();
        }

        [[nodiscard]] constexpr const std::type_info& type_info() const noexcept {
            if (meta_ == nullptr) [[unlikely]] {
                return typeid(void);
            }
            return *meta_->type_info();
        }

        /**
         * @note: If T is not a decayed type, this method will almost certainly return false,
         *        since meta_ is built from the decayed original type.
         */
        template <typename T>
        [[nodiscard]] constexpr bool is() const noexcept {
            return meta_ == &meta_for<T>;
        }

        [[nodiscard]] constexpr const void* data() const noexcept {
            return ptr_;
        }

        [[nodiscard]] bool same_type(const any_view& other) const noexcept {
            return meta_ == other.meta_;
        }

        /********************* ANY_CAST OVERLOADS *********************/

        template <typename T>
        friend const std::remove_cvref_t<T>* any_cast(const any_view* op) noexcept;

        template <typename T>
        friend const std::remove_cvref_t<T>& any_cast(const any_view& op);

        template <typename T>
        friend const std::remove_cvref_t<T>* unchecked_any_cast(const any_view* op) noexcept;

        template <typename T>
        friend const std::remove_cvref_t<T>& unchecked_any_cast(const any_view& op) noexcept;

        /********************* COMPARISONS *********************/

        friend bool operator==(const any_view& lhs, const any_view& rhs) noexcept {
            return lhs.same_type(rhs) && lhs.ptr_ == rhs.ptr_;
        }

        constexpr friend bool operator==(const any_view& view, std::nullptr_t) noexcept {
            return view.ptr_ == nullptr;
        }

        constexpr friend bool operator==(std::nullptr_t, const any_view& view) noexcept {
            return view.ptr_ == nullptr;
        }
    };

    /********************* ANY_CAST OVERLOADS *********************/

    template <typename T>
    const std::remove_cvref_t<T>* any_cast(const any_view* op) noexcept {
        using U = std::remove_cvref_t<T>;
        if (op->ptr_ && op->meta_ == &any_view::meta_for<U>) {
            return static_cast<const U*>(op->ptr_);
        }
        return nullptr;
    }

    template <typename T>
    const std::remove_cvref_t<T>& any_cast(const any_view& op) {
        using U = std::remove_cvref_t<T>;
        if (!op.ptr_ || op.meta_ != any_view::meta_for<U>) {
            throw std::bad_any_cast();
        }
        return *static_cast<const U*>(op.ptr_);
    }

    template <typename T>
    const std::remove_cvref_t<T>* unchecked_any_cast(const any_view* op) noexcept {
        return static_cast<const T*>(op->ptr_);
    }

    template <typename T>
    const std::remove_cvref_t<T>& unchecked_any_cast(const any_view& op) noexcept {
        return *unchecked_any_cast<T>(&op);
    }

    /********************* FACTORY METHODS *********************/

    template <typename T>
    requires (!std::is_pointer_v<std::remove_cvref_t<T>>)
    [[nodiscard]] constexpr any_view make_any_view(T&& val) noexcept {
        return any_view{val};
    }

    template <typename T>
    [[nodiscard]] constexpr any_view make_any_view(const T* ptr) noexcept {
        return any_view{ptr};
    }

    constexpr void swap(any_view& lhs, any_view& rhs) noexcept {
        lhs.swap(rhs);
    }

} // namespace urlicht

#endif // URLICHT_ANY_VIEW_H
