
#ifndef URLICHT_INPLACE_ANY_H
#define URLICHT_INPLACE_ANY_H

#include <type_traits>
#include <memory>
#include <any>
#include <urlicht/any/detail/utils.h>

namespace urlicht {

    /**
    * @brief Stack-allocated type-erased storage with fixed capacity
    *
    * @tparam MaxSize Maximum size of stored objects
    * @tparam MaxAlign Maximum alignment of stored objects
    *
    * @note Compile-time error if T is too large or misaligned.
    */
    template <std::size_t MaxSize, std::size_t MaxAlign = alignof(std::max_align_t)>
    class inplace_any;

    namespace any_detail {
        template <typename T>
        struct is_inplace_any {
            static constexpr bool value = false;
        };

        template <std::size_t MaxSize, std::size_t MaxAlign>
        struct is_inplace_any<inplace_any<MaxSize, MaxAlign>> {
            static constexpr bool value = true;
        };
    }

    template <typename T>
    inline constexpr bool is_inplace_any_v = any_detail::is_inplace_any<T>::value;

    namespace concepts {
        template <typename T>
        concept inplace_any = is_inplace_any_v<T>;
    }


    template <std::size_t MaxSize, std::size_t MaxAlign>
    class inplace_any {
    public:
        using storage_type = std::byte[MaxSize];

    private:
        template <typename T>
        static constexpr bool can_fit_storage = sizeof(T) <= MaxSize && alignof(T) <= MaxAlign;

        template <typename T> // Guaranteed to be decayed type
        static constexpr any_detail::vtable vtable_for = {
            .destroy = [](void* src) noexcept {
                if constexpr (!std::is_trivially_destructible_v<T>) {
                    auto* storage = static_cast<T*>(src);
                    std::destroy_at(storage);
                }
            },
            .move = [](void* src, void* dst)
            noexcept(std::is_nothrow_move_constructible_v<T>) {
                auto* value = static_cast<T*>(src);
                std::construct_at(static_cast<T*>(dst), std::move(*value));
                std::destroy_at(value);
            },
            .clone = [](const void* src, void* dst) {
                auto* value = static_cast<const T*>(src);
                std::construct_at(static_cast<T*>(dst), *value);
            },
            .type_info = [] noexcept {
                return &typeid(T);
            }
        };

        template <typename T, typename... Args>
        constexpr void construct(const bool cond, Args&& ...args)
        noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<T>, Args&&...>) {
            using U = std::remove_cvref_t<T>;
            static_assert(can_fit_storage<U>, "The provided type is too large or misaligned");
            static_assert(std::copy_constructible<U>, "The provided type must be copy constructible");
            if (cond) {
                std::construct_at(reinterpret_cast<U*>(data_), std::forward<Args>(args)...);
                vtable_ = &vtable_for<U>;
            }
        }

        constexpr void copy_from(const inplace_any& other) {
            other.vtable_->clone(other.data_, data_);
            vtable_ = other.vtable_;
        }

        constexpr void move_from(inplace_any&& other) {
            other.vtable_->move(other.data_, data_);
            vtable_ = other.vtable_;
            other.vtable_ = nullptr;
        }

        // Data member
        alignas(MaxAlign) storage_type data_;
        const any_detail::vtable* vtable_{};

    public:
        static consteval std::size_t max_size() noexcept { return MaxSize; }
        static consteval std::size_t max_align() noexcept { return MaxAlign; }

        /************************** CONSTRUCTORS ***************************/

        constexpr inplace_any() noexcept = default;

        template <typename T>
        requires (!is_inplace_any_v<std::remove_cvref_t<T>> && std::constructible_from<T, T&&>)
        constexpr inplace_any(T&& val)
        noexcept(std::is_nothrow_constructible_v<T, T&&>){
            this->template construct<T>(true, std::forward<T>(val));
        }

        template <typename T>
        requires (std::constructible_from<T, T&&>)
        constexpr inplace_any(const bool cond, T&& val)
        noexcept(std::is_nothrow_constructible_v<T, T&&>) {
            this->template construct<T>(cond, std::forward<T>(val));
        }

        template <typename T, typename... Args>
        requires (!is_inplace_any_v<std::remove_cvref_t<T>>) &&
                  std::constructible_from<std::remove_cvref_t<T>, Args&&...>
        constexpr inplace_any(any_detail::inplace_t<T>, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
            this->template construct<T>(true, std::forward<Args>(args)...);
        }

        template <typename T, typename... Args>
        requires (!is_inplace_any_v<std::remove_cvref_t<T>>) &&
                  std::constructible_from<std::remove_cvref_t<T>, Args&&...>
        explicit constexpr inplace_any(any_detail::inplace_cond_t<T>, const bool cond, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
            this->template construct<T>(cond, std::forward<Args>(args)...);
        }

        constexpr inplace_any(const inplace_any& other) {
            if (other.has_value()) {
                this->copy_from(other);
            }
        }

        constexpr inplace_any(inplace_any&& other) noexcept {
            if (other.has_value()) {
                this->move_from(std::move(other));
            }
        }

        constexpr inplace_any& operator=(const inplace_any& other) {
            if (this != &other) {
                this->reset();
                if (other.has_value()) {
                    this->copy_from(other);
                }
            }
            return *this;
        }

        constexpr inplace_any& operator=(inplace_any&& other) noexcept {
            if (this != &other) {
                this->reset();
                if (other.has_value()) {
                    this->move_from(std::move(other));
                }
            }
            return *this;
        }

        constexpr ~inplace_any() noexcept {
            this->reset();
        }

        /************************** MODIFIERS ***************************/

        template <typename T, typename... Args>
        requires std::is_constructible_v<T, Args...>
        constexpr std::remove_cvref_t<T>& emplace(Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args...>) {
            using U = std::remove_cvref_t<T>;
            static_assert(can_fit_storage<U>, "The provided type is too large or misaligned");
            static_assert(std::copy_constructible<U>, "The provided type must be copy constructible");
            this->reset();
            auto* obj = std::construct_at(reinterpret_cast<U*>(data_), std::forward<Args>(args)...);
            vtable_ = &vtable_for<U>;
            return *obj;
        }

        constexpr void reset() noexcept {
            if (this->has_value()) {
                vtable_->destroy(data_);
                vtable_ = nullptr;
            }
        }

        /**
         * @note: No exception guarantee is provided. Optimally, you should ensure that
         *        all referenced object type is nothrow move constructible.
         */
        constexpr void swap(inplace_any& other) noexcept(false) {
            if (this == &other) {
                return;
            }
            if (!this->has_value() && !other.has_value()) { // Both empty
                return;
            }

            if (!other.has_value()) {
                other.move_from(std::move(*this));
            } else if (!this->has_value()) {
                this->move_from(std::move(other));
            } else {
                inplace_any tmp{std::move(other)};
                other.move_from(std::move(*this));
                this->move_from(std::move(tmp));
            }
        }

        /************************** OBSERVERS ***************************/

        [[nodiscard]] constexpr bool has_value() const noexcept {
            return vtable_ != nullptr;
        }

        [[nodiscard]] constexpr operator bool() const noexcept {
            return has_value();
        }

        [[nodiscard]] const std::type_info& type_info() const noexcept {
            if (this->has_value()) {
                return *vtable_->type_info();
            }
            return typeid(void);
        }

        template <typename T>
        [[nodiscard]] bool is() const noexcept {
            return typeid(T) == type_info();
        }

        template <typename T, std::size_t S, std::size_t A>
        friend const std::remove_cvref_t<T>* any_cast(const inplace_any<S, A>* operand) noexcept;

        template <typename T, std::size_t S, std::size_t A>
        friend std::remove_cvref_t<T>* any_cast(inplace_any<S, A>* operand) noexcept;

        template <typename T, std::size_t S, std::size_t A>
        friend std::remove_cvref_t<T>* unchecked_any_cast(inplace_any<S, A>* operand) noexcept;

    };

    // Non-member swap
    template <std::size_t S, std::size_t A>
        constexpr void swap(inplace_any<S, A>& lhs, inplace_any<S, A>& rhs) noexcept {
        lhs.swap(rhs);
    }

    /************************** ANY_CAST OVERLOADS ***************************/

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>* any_cast(inplace_any<S, A>* operand) noexcept {
        using U = std::remove_cvref_t<T>;
        if (operand && operand->has_value() && operand->type_info() == typeid(U)) {
            return reinterpret_cast<U*>(operand->data_);
        }
        return nullptr;
    }

    template <typename T, std::size_t S, std::size_t A>
    const std::remove_cvref_t<T>* any_cast(const inplace_any<S, A>* operand) noexcept {
        return any_cast<T>(const_cast<inplace_any<S, A>*>(operand));
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>& any_cast(inplace_any<S, A>& operand) {
        auto* ptr = any_cast<T>(&operand);
        if (!ptr) {
            throw std::bad_any_cast();
        }
        return *ptr;
    }

    template <typename T, std::size_t S, std::size_t A>
    const std::remove_cvref_t<T>& any_cast(const inplace_any<S, A>& operand) {
        return any_cast<T>(const_cast<inplace_any<S, A>&>(operand));
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>&& any_cast(inplace_any<S, A>&& operand) {
        return std::move(any_cast<T>(operand));
    }

    /******************** UNCHECKED_ANY_CAST OVERLOADS **********************/

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>* unchecked_any_cast(inplace_any<S, A>* operand) noexcept {
        using U = std::remove_cvref_t<T>;
        return reinterpret_cast<U*>(operand->data_);
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>& unchecked_any_cast(inplace_any<S, A>& operand) noexcept {
        return *unchecked_any_cast<T>(&operand);
    }

    template <typename T, std::size_t S, std::size_t A>
    const std::remove_cvref_t<T>& unchecked_any_cast(const inplace_any<S, A>& operand) noexcept {
        return *unchecked_any_cast<T>(&const_cast<inplace_any<S, A>&>(operand));
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>&& unchecked_any_cast(inplace_any<S, A>&& operand) noexcept {
        return std::move(*unchecked_any_cast<T>(&operand));
    }

    // make_inplace_any
    template <typename T,
              typename... Args,
              std::size_t S = std::max(sizeof(T) * 2, 16ull),
              std::size_t A = alignof(T)>
    [[nodiscard]] constexpr auto make_inplace_any(Args&&... args) {
        inplace_any<S, A> any(inplace<T>, std::forward<Args>(args)...);
        return any;
    }
}



#endif //URLICHT_INPLACE_ANY_H
