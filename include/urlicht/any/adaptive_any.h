#ifndef URLICHT_ADAPTIVE_ANY
#define URLICHT_ADAPTIVE_ANY

#include <urlicht/any/detail/utils.h>
#include <urlicht/concepts_utility.h>
#include <memory>
#include <type_traits>

namespace urlicht {

    /**
    * @class adaptive_any
    * @brief A type-safe container for single values of any type, with customizable
    *        small buffer optimization.
    *
    * @tparam OptimizeForSize The size of the internal buffer for SBO. Objects smaller
    *         than or equal to this size will be stored internally.
    * @tparam OptimizeForAlign The alignment requirement for the internal buffer.
    */
    template <std::size_t OptimizeForSize, std::size_t OptimizeForAlign = alignof(std::max_align_t)>
    class adaptive_any;

    namespace any_detail {
        template <typename Other>
        struct is_adaptive_any {
            constexpr static bool value = false;
        };

        template <std::size_t OptimizeForSize, std::size_t OptimizeForAlign>
        struct is_adaptive_any<adaptive_any<OptimizeForSize, OptimizeForAlign>> {
            constexpr static bool value = true;
        };
    }

    template <typename T>
    inline constexpr bool is_adaptive_any_v = any_detail::is_adaptive_any<T>::value;

    namespace concepts {
        template <typename T>
        concept adaptive_any = is_adaptive_any_v<T>;
    }


    template <std::size_t OptimizeForSize, std::size_t OptimizeForAlign>
    class adaptive_any {
    private:
        using storage_type = union {
            alignas(OptimizeForAlign) std::byte aligned_buffer [OptimizeForSize];
            void* heap_ptr;
        };

        template <typename VTy>
        static constexpr bool use_sbo_v = any_detail::is_small_obj_v<VTy, OptimizeForSize, OptimizeForAlign>;

        template <typename T, bool InSBO>
        static constexpr any_detail::vtable vtable_for = {
            .destroy = [](void* src) noexcept {
                auto* storage = static_cast<storage_type*>(src);
                if constexpr (InSBO) {
                    if constexpr (!std::is_trivially_destructible_v<T>) {
                        std::destroy_at(reinterpret_cast<T*>(storage->aligned_buffer));
                    }
                } else {
                    std::default_delete<T>{}(reinterpret_cast<T*>(storage->heap_ptr));
                }
            },
            // If the object may throw in move construction, it is placed on the heap to
            // ensure nothrow move operation of adaptive_any
            .move = [](void* src, void* dest) noexcept {
                auto* src_ = static_cast<storage_type*>(src);
                auto* dest_ = static_cast<storage_type*>(dest);
                if constexpr (InSBO) {
                    auto* src_value = reinterpret_cast<T*>(src_->aligned_buffer);
                    std::construct_at(reinterpret_cast<T*>(dest_->aligned_buffer), std::move(*src_value));
                    std::destroy_at(src_value);
                } else {
                    dest_->heap_ptr = src_->heap_ptr;
                    src_->heap_ptr = nullptr;
                }
            },
            .clone = [](const void* src, void* dest) {
                auto* src_ = static_cast<storage_type*>(src);
                auto* dest_ = static_cast<storage_type*>(dest);
                if constexpr (InSBO) {
                    auto* src_value = reinterpret_cast<T*>(src_->aligned_buffer);
                    std::construct_at(reinterpret_cast<T*>(dest_->aligned_buffer), *src_value);
                } else {
                    auto* src_value = reinterpret_cast<T*>(src_->heap_ptr);
                    dest_->heap_ptr = new T(*src_value);
                }
            },
            .type_info = []() noexcept { return &typeid(T); },
            .in_sbo = []() noexcept { return InSBO; }
        };

        template <typename T, typename... Args>
        constexpr void construct(const bool cond, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<T>, Args&&...>) {
            using U = std::remove_cvref_t<T>;
            static_assert(std::copy_constructible<U>, "The provided type must be copy constructible");
            if (cond) {
                if constexpr (use_sbo_v<U>) {
                    std::construct_at(
                        reinterpret_cast<U*>(content_.aligned_buffer),
                        std::forward<Args>(args)...);
                } else {
                    content_.heap_ptr = new U(std::forward<Args>(args)...);
                }
                vtable_ = &vtable_for<U, use_sbo_v<U>>;
            }
        }

        constexpr void copy_from(const adaptive_any& other) {
            other.vtable_->clone(other.content_, content_);
            vtable_ = other.vtable_;
        }

        constexpr void move_from(adaptive_any&& other) noexcept {
            other.vtable_->move(other.content_, content_);
            vtable_ = other.vtable_;
            other.vtable_ = nullptr;
        }

    public:
        static consteval std::size_t buffer_size() noexcept { return OptimizeForSize; }
        static consteval std::size_t buffer_align() noexcept { return OptimizeForAlign; }

        /************************* CONSTRUCTORS **************************/

        constexpr adaptive_any() noexcept = default;

        template <typename T>
        requires (!is_adaptive_any_v<std::remove_cvref_t<T>>) &&
                  std::constructible_from<T, T&&>
        constexpr adaptive_any(T&& val)
        noexcept(std::is_nothrow_constructible_v<T, T&&>) {
            this->template construct<T>(true, std::forward<T>(val));
        }

        template <typename T>
        requires (!is_adaptive_any_v<std::remove_cvref_t<T>>) &&
                  std::constructible_from<T, T&&>
        constexpr adaptive_any(const bool cond, T&& val)
        noexcept(std::is_nothrow_constructible_v<T, T&&>) {
            this->template construct<T>(cond, std::forward<T>(val));
        }

        template <typename T, typename... Args>
        requires (!is_adaptive_any_v<std::remove_cvref_t<T>>) &&
                  std::constructible_from<std::remove_cvref_t<T>, Args&&...>
        constexpr adaptive_any(any_detail::inplace_t<T>, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
            this->template construct<T>(true, std::forward<Args>(args)...);
        }

        template <typename T, typename... Args>
        requires (!is_adaptive_any_v<std::remove_cvref_t<T>>) &&
                  std::constructible_from<std::remove_cvref_t<T>, Args&&...>
        explicit constexpr adaptive_any(any_detail::inplace_cond_t<T>, const bool cond, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
            this->template construct<T>(cond, std::forward<Args>(args)...);
        }

        constexpr adaptive_any(const adaptive_any& other) {
            if (other.has_value()) {
                this->copy_from(other);
            }
        }

        constexpr adaptive_any(adaptive_any&& other) noexcept {
            if (other.has_value()) {
                this->move_from(std::move(other));
            }
        }

        constexpr adaptive_any& operator=(const adaptive_any& other) {
            if (this != &other) {
                this->reset();
                if (other.has_value()) {
                    this->copy_from(other);
                }
            }
            return *this;
        }

        constexpr adaptive_any& operator=(adaptive_any&& other) noexcept {
            if (this != &other) {
                this->reset();
                if (other.has_value()) {
                    this->move_from(std::move(other));
                }
            }
            return *this;
        }

        constexpr ~adaptive_any() noexcept {
            this->reset();
        }

        /************************* MODIFIERS **************************/

        constexpr void reset() noexcept {
            if (this->has_value()) {
                vtable_->destroy(content_);
                vtable_ = nullptr;
            }
        }

        template <typename T, typename... Args>
        requires std::constructible_from<T, Args&&...>
        constexpr std::remove_cvref_t<T>& emplace(Args&&... args) {
            using U = std::remove_cvref_t<T>;
            static_assert(std::copy_constructible<U>, "The provided type must be copy constructible");
            this->reset();
            this->template construct<U>(true, std::forward<Args>(args)...);
            if constexpr (use_sbo_v<U>) {
                return *reinterpret_cast<U*>(content_.aligned_buffer);
            } else {
                return *reinterpret_cast<U*>(content_.heap_ptr);
            }
        }

        constexpr void swap(adaptive_any& other) noexcept {
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
                adaptive_any tmp{std::move(other)};
                other.move_from(std::move(*this));
                this->move_from(std::move(tmp));
            }
        }

        /************************* OBSERVERS **************************/

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

        [[nodiscard]] constexpr bool in_sbo() const noexcept {
            if (this->has_value()) {
                return vtable_->in_sbo();
            }
            return false;
        }

        // any_casts
        template <typename T, std::size_t S, std::size_t A>
        friend const T* any_cast(const adaptive_any<S, A>* operand) noexcept;

        template <typename T, std::size_t S, std::size_t A>
        friend T* any_cast(adaptive_any<S, A>* operand) noexcept;

        template <typename T, std::size_t S, std::size_t A>
        friend T* unchecked_any_cast(adaptive_any<S, A>* operand) noexcept;

    private:
        // Data member
        storage_type content_;
        any_detail::vtable* vtable_{};
    };


    // Non-member swap
    template <std::size_t S, std::size_t A>
    constexpr void swap(adaptive_any<S, A>& lhs, adaptive_any<S, A>& rhs) noexcept {
        lhs.swap(rhs);
    }

    /************************** ANY_CAST OVERLOADS ***************************/

    /**
     *@note: This ignores all cvref qualifiers of T
     */
    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>* any_cast(adaptive_any<S, A>* operand) noexcept {
        using U = std::remove_cvref_t<T>;
        if (operand && operand->has_value() && operand->type_info() == typeid(U)) {
            if constexpr (adaptive_any<S, A>::template use_sbo_v<U>) {
                return reinterpret_cast<U*>(operand->content_.aligned_buffer);
            } else {
                return reinterpret_cast<U*>(operand->content_.heap_ptr);
            }
        }
        return nullptr;
    }


    template <typename T, std::size_t S, std::size_t A>
    const std::remove_cvref_t<T>* any_cast(const adaptive_any<S, A>* operand) noexcept {
        return any_cast<T>(const_cast<adaptive_any<S, A>*>(operand));
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>& any_cast(adaptive_any<S, A>& operand) {
        auto* ptr = any_cast<T>(&operand);
        if (!ptr) {
            throw std::bad_any_cast();
        }
        return *ptr;
    }

    template <typename T, std::size_t S, std::size_t A>
    const std::remove_cvref_t<T>& any_cast(const adaptive_any<S, A>& operand) {
        return any_cast<T>(const_cast<adaptive_any<S, A>&>(operand));
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>&& any_cast(adaptive_any<S, A>&& operand) {
        return std::move(any_cast<T>(operand));
    }

    /******************** UNCHECKED_ANY_CAST OVERLOADS **********************/

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>* unchecked_any_cast(adaptive_any<S, A>* operand) noexcept {
        using U = std::remove_cvref_t<T>;
        if constexpr (adaptive_any<S, A>::template use_sbo_v<U>) {
            return reinterpret_cast<U*>(operand->content_.aligned_buffer);
        } else {
            return reinterpret_cast<U*>(operand->content_.heap_ptr);
        }
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>& unchecked_any_cast(adaptive_any<S, A>& operand) noexcept {
        return *unchecked_any_cast<T>(&operand);
    }

    template <typename T, std::size_t S, std::size_t A>
    const std::remove_cvref_t<T>& unchecked_any_cast(const adaptive_any<S, A>& operand) noexcept {
        return *unchecked_any_cast<T>(&const_cast<adaptive_any<S, A>&>(operand));
    }

    template <typename T, std::size_t S, std::size_t A>
    std::remove_cvref_t<T>&& unchecked_any_cast(adaptive_any<S, A>&& operand) noexcept {
        return std::move(*unchecked_any_cast<T>(&operand));
    }

    // make_adaptive_any
    template <typename T,
              typename... Args,
              std::size_t S = std::max(sizeof(T) * 2, 16ull),
              std::size_t A = alignof(T)>
    [[nodiscard]] constexpr auto make_adaptive_any(Args&&... args) {
        adaptive_any<S, A> any(inplace<T>, std::forward<Args>(args)...);
        return any;
    }

}
#endif //URLICHT_ADAPTIVE_ANY
