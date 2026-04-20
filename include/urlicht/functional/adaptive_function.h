#ifndef URLICHT_ADAPTIVE_FUNCTION_H
#define URLICHT_ADAPTIVE_FUNCTION_H

#include <urlicht/internal/tag.h>
#include <urlicht/config.h>
#include <memory>
#include <type_traits>
#include <functional>
#include <urlicht/concepts_utility.h>

namespace urlicht {
    /**
     * @brief A specifier-aware polymorphic function wrapper with customizable small-object optimization
     *        (SBO) size and alignment. Functional objects are stored inline when they meet the size and alignment
     *        requirements. Otherwise, they are allocated on the heap as a fallback.
     *
     * @tparam T Function signature. Supports all valid combinations of noexcept, &/&&, and const.
     * @tparam OptimizeForSize SBO size. Defaults to 64.
     * @tparam OptimizeForAlign Alignment size. Defaults to {alignof(std::max_align_t)}.
     */
    template <typename T, size_t OptimizeForSize = 64u, size_t OptimizeForAlign = alignof(std::max_align_t)>
    class adaptive_function;

    namespace functional_detail {

        template <typename>
        struct is_urlicht_adaptive_function : std::false_type {};

        template <typename Sig, size_t S, size_t A>
        struct is_urlicht_adaptive_function<adaptive_function<Sig, S, A>> : std::true_type {};

        template <typename T, size_t Size, size_t Align, bool IsConst,
                  bool IsLvalue, bool IsRvalue, bool IsNoexcept>
        class adaptive_function_base;

        template <typename R, size_t Size, size_t Align, bool IsConst,
                  bool IsLvalue, bool IsRvalue, bool IsNoexcept, typename ...Args>
        class adaptive_function_base<R(Args...), Size, Align, IsConst, IsLvalue, IsRvalue, IsNoexcept> {
        private:
            using self_type = adaptive_function_base;

            union storage_t {
                void* heap_ptr;
                alignas(Align) std::byte aligned_buffer[Size];
            };

            template <typename T>
            static constexpr bool use_sbo_v =
                sizeof(T) <= Size && alignof(T) <= Align && std::is_nothrow_move_constructible_v<T>;

            template <typename T>
            static consteval bool is_callable_from() {
                using U = std::remove_cv_t<T>;
                using BaseU = std::conditional_t<IsConst, const U, U>;
                if constexpr (IsNoexcept) {
                    if constexpr (IsLvalue) {
                        return std::is_nothrow_invocable_r_v<R, BaseU&, Args...>;
                    } else if constexpr (IsRvalue) {
                        return std::is_nothrow_invocable_r_v<R, BaseU, Args...>;
                    } else {
                        return std::is_nothrow_invocable_r_v<R, BaseU, Args...> &&
                               std::is_nothrow_invocable_r_v<R, BaseU&, Args...>;
                    }
                } else {
                    if constexpr (IsLvalue) {
                        return std::is_invocable_r_v<R, BaseU&, Args...>;
                    } else if constexpr (IsRvalue) {
                        return std::is_invocable_r_v<R, BaseU, Args...>;
                    } else {
                        return std::is_invocable_r_v<R, BaseU, Args...> &&
                               std::is_invocable_r_v<R, BaseU&, Args...>;
                    }
                }
            }

            template <typename T>
            static constexpr bool is_callable_from_v = is_callable_from<T>();

            using storage_arg_t = std::conditional_t<IsConst, const storage_t&, storage_t&>;
            using call_func = R(*)(storage_arg_t, Args...) noexcept(IsNoexcept);

            struct vtable_t {
                call_func call = nullptr;
                void (*clone)(const storage_t&, storage_t&) = nullptr;
                void (*move)(storage_t&, storage_t&) noexcept = nullptr;
                void (*destroy)(storage_t&) noexcept = nullptr;
                bool (*in_sbo)() noexcept = nullptr;
            };

            template <typename T, bool InSBO>
            static constexpr vtable_t vtable_for = {
                .call = [](storage_arg_t storage, Args... args) noexcept(IsNoexcept) -> R {
                    using CTy = std::conditional_t<IsConst, const T, T>;
                    CTy* actual;
                    if constexpr (InSBO) {
                        actual = reinterpret_cast<CTy*>(storage.aligned_buffer);
                    } else {
                        UL_ASSERT(storage.heap_ptr != nullptr, "Call on null heap pointer");
                        actual = static_cast<CTy*>(storage.heap_ptr);
                    }
                    if constexpr (IsRvalue) {
                        return std::invoke(std::move(*actual), std::forward<Args>(args)...);
                    } else {
                        return std::invoke(*actual, std::forward<Args>(args)...);
                    }
                },
                .clone = [](const storage_t& src, storage_t& dest) {
                    if constexpr (InSBO) {
                        const auto* obj = reinterpret_cast<const T*>(src.aligned_buffer);
                        std::construct_at(reinterpret_cast<T*>(dest.aligned_buffer), *obj);
                    } else {
                        dest.heap_ptr = new T(*static_cast<T*>(src.heap_ptr));
                    }
                },
                // Note: The adaptive_function being moved from is cleared subsequently for safety reasons.
                //       Do NOT change this.
                .move = [](storage_t& src, storage_t& dest) noexcept {
                    if constexpr (InSBO) {
                        auto* obj = reinterpret_cast<T*>(src.aligned_buffer);
                        std::construct_at(reinterpret_cast<T*>(dest.aligned_buffer), std::move(*obj));
                        std::destroy_at(obj);
                    } else {
                        dest.heap_ptr = src.heap_ptr;
                        src.heap_ptr = nullptr;
                    }
                },
                .destroy = [](storage_t& storage) noexcept -> void {
                    if constexpr (InSBO) {
                        std::destroy_at(reinterpret_cast<T*>(storage.aligned_buffer));
                    } else {
                        delete static_cast<T*>(storage.heap_ptr);
                    }
                },
                .in_sbo = []() noexcept {
                    return InSBO;
                }
            };

            template <auto f>
            static constexpr vtable_t vtable_for_nontype = {
                .call = [](storage_arg_t, Args... args) noexcept(IsNoexcept) -> R {
                    return std::invoke(f, std::forward<Args>(args)...);
                },
                .clone = [](const storage_t&, storage_t&) { },
                .move = [](storage_t&, storage_t&) noexcept { },
                .destroy = [](storage_t&) noexcept { },
                .in_sbo = [] () noexcept { return false; }
            };

            template <typename T, typename ...CArgs>
            void construct_from(CArgs&&... args)
            noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<T>, CArgs...>) {
                using U = std::remove_cvref_t<T>;
                if constexpr (use_sbo_v<U>) {
                    std::construct_at(
                        reinterpret_cast<U*>(storage_.aligned_buffer),
                        std::forward<CArgs>(args)...);
                } else {
                    storage_.heap_ptr = new U(std::forward<CArgs>(args)...);
                }
                vtable_ = &vtable_for<U, use_sbo_v<U>>;
            }

            void copy_from(const adaptive_function_base& other) {
                other.vtable_->clone(other.storage_, this->storage_);
                this->vtable_ = other.vtable_;
            }

            void move_from(adaptive_function_base&& other) noexcept {
                other.vtable_->move(other.storage_, this->storage_);
                this->vtable_ = other.vtable_;
                other.vtable_ = nullptr;
            }

            // Data members
            storage_t storage_;
#if UL_HAS_CPP26
    [[indeterminate]]
#endif
            const vtable_t* vtable_{};

        public:
            // Constructors
            constexpr adaptive_function_base() noexcept = default;
            constexpr adaptive_function_base(std::nullptr_t) noexcept {}

            /**
             * @brief Constructs from the given callable object.
             * @param func The callable to store. Requirements:
             *        1. It must be copy-constructible.
             *        2. It must be compatible with the specifiers.
             *        3. It must not be of the same specialization of adaptive_function.
             */
            template <typename F>
            requires is_callable_from_v<F> &&
                     (!std::is_base_of_v<self_type, std::remove_cvref_t<F>>) &&
                     std::constructible_from<std::remove_cvref_t<F>, F&&>
            constexpr adaptive_function_base(F&& func)
            noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>) {
                static_assert(std::copy_constructible<std::remove_cvref_t<F>>, "F must be copyable");
                if constexpr (requires() { func == nullptr; }) {
                    if (func == nullptr) [[unlikely]] {
                        return;
                    }
                }
                this->template construct_from<F>(std::forward<F>(func));
            }

            /**
             * @brief Constructs the designated type in-place from the given arguments.
             */
            template <typename F, typename... _Args>
            requires is_callable_from_v<F> &&
                     (!std::is_base_of_v<self_type, F>) &&
                     std::constructible_from<F, _Args&&...>
            constexpr adaptive_function_base(detail::inplace_t<F>, _Args&&... args)
            noexcept(std::is_nothrow_constructible_v<F, _Args&&...>) {
                static_assert(concepts::decayed<F>, "F must be a decayed type");
                static_assert(std::copy_constructible<F>, "F must be copyable");
                this->template construct_from<F>(std::forward<_Args>(args)...);
            }

            /**
             * @brief Constructs the designated type in-place from an initializer-list and other given arguments.
             */
            template <typename F, typename C, typename ..._Args>
            requires is_callable_from_v<F> &&
                     (!std::is_base_of_v<self_type, F>) &&
                     std::constructible_from<F, std::initializer_list<C>, _Args&&...>
            constexpr explicit adaptive_function_base(
                detail::inplace_t<F>, std::initializer_list<C> il, _Args&&... args)
            noexcept(std::is_nothrow_constructible_v<F, std::initializer_list<C>, _Args&&...>) {
                static_assert(concepts::decayed<F>, "F must be a decayed type");
                static_assert(std::copy_constructible<F>, "F must be copyable");
                this->template construct_from<F>(il, std::forward<_Args>(args)...);
            }

            /**
             * @brief Constructs with a NTTP wrapped by {nontype}.
             *        Example usage: adaptive_function func{nontype<func_ptr>};
             */
            template <auto f>
            requires is_callable_from_v<decltype(f)>
            constexpr adaptive_function_base(detail::nontype_t<f>) noexcept {
                this->vtable_ = &vtable_for_nontype<f>;
            }

            constexpr adaptive_function_base(const adaptive_function_base& other) {
                if (other) [[likely]] {
                    this->copy_from(other);
                }
            }

            constexpr adaptive_function_base(adaptive_function_base&& other) noexcept {
                if (other) [[likely]] {
                    this->move_from(std::move(other));
                }
            }

            constexpr adaptive_function_base& operator=(const adaptive_function_base& other) {
                if (this != &other) [[likely]] {
                    this->reset();
                    if (other) [[likely]] {
                        this->copy_from(other);
                    }
                }
                return *this;
            }

            constexpr adaptive_function_base& operator=(adaptive_function_base&& other) noexcept {
                if (this != &other) [[likely]] {
                    this->reset();
                    if (other) [[likely]] {
                        this->move_from(std::move(other));
                    }
                }
                return *this;
            }

            constexpr adaptive_function_base& operator=(std::nullptr_t) noexcept {
                this->reset();
                return *this;
            }

            template <typename F>
            requires is_callable_from_v<F> &&
                     std::constructible_from<std::remove_cvref_t<F>, F&&> &&
                     (!std::is_base_of_v<self_type, std::remove_cvref_t<F>>)
            constexpr adaptive_function_base& operator=(F&& func)
            noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<F>, F&&>) {
                static_assert(std::copy_constructible<std::remove_cvref_t<F>>, "F must be copyable");
                this->reset();
                if constexpr (requires() { func == nullptr; }) {
                    if (func == nullptr) [[unlikely]] {
                        return *this;
                    }
                }
                this->template construct_from<F>(std::forward<F>(func));
                return *this;
            }

            constexpr ~adaptive_function_base() noexcept {
                this->reset();
            }

            // operator ()
            R operator() (Args... args)
            requires (!IsConst) && (!IsLvalue) && (!IsRvalue) && (!IsNoexcept) {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) const
            requires IsConst && (!IsLvalue) && (!IsRvalue) && (!IsNoexcept) {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) &
            requires (!IsConst) && IsLvalue && (!IsRvalue) && (!IsNoexcept) {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) &&
            requires (!IsConst) && (!IsLvalue) && IsRvalue && (!IsNoexcept) {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) noexcept
            requires (!IsConst) && (!IsLvalue) && (!IsRvalue) && IsNoexcept {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) & noexcept
            requires (!IsConst) && IsLvalue && (!IsRvalue) && IsNoexcept {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) && noexcept
            requires (!IsConst) && (!IsLvalue) && IsRvalue && IsNoexcept {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) const &
            requires IsConst && IsLvalue && (!IsRvalue) && (!IsNoexcept) {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) const &&
            requires IsConst && (!IsLvalue) && IsRvalue && (!IsNoexcept) {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) const noexcept
            requires IsConst && (!IsLvalue) && (!IsRvalue) && IsNoexcept {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) const & noexcept
            requires IsConst && IsLvalue && (!IsRvalue) && IsNoexcept {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            R operator() (Args... args) const && noexcept
            requires IsConst && (!IsLvalue) && IsRvalue && IsNoexcept {
                UL_ASSERT(this->vtable_ != nullptr, "adaptive_function is null");
                return this->vtable_->call(storage_, std::forward<Args>(args)...);
            }

            template <typename F, typename ... CArgs>
            requires std::constructible_from<std::remove_cvref_t<F>, CArgs&&...> &&
                     is_callable_from_v<F>
            constexpr std::remove_cvref_t<F>& emplace(CArgs&& ...cargs)
            noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<F>, CArgs&&...>) {
                using U = std::remove_cvref_t<F>;
                static_assert(std::copy_constructible<U>, "F must be copyable");

                this->reset();
                this->template construct_from<U>(std::forward<CArgs>(cargs)...);

                if constexpr (use_sbo_v<U>) {
                    return *reinterpret_cast<U*>(this->storage_.aligned_buffer);
                } else {
                    return *static_cast<U*>(this->storage_.heap_ptr);
                }
            }

            template <typename F, typename T, typename ... CArgs>
            requires std::constructible_from<std::remove_cvref_t<F>, CArgs&&...> &&
                     is_callable_from_v<F>
            constexpr std::remove_cvref_t<F>& emplace(std::initializer_list<T> il, CArgs&& ...cargs)
            noexcept(std::is_nothrow_constructible_v<std::remove_cvref_t<F>, CArgs&&...>) {
                using U = std::remove_cvref_t<F>;
                static_assert(std::copy_constructible<U>, "F must be copyable");

                this->reset();
                this->template construct_from<U>(il, std::forward<CArgs>(cargs)...);

                if constexpr (use_sbo_v<U>) {
                    return *reinterpret_cast<U*>(this->storage_.aligned_buffer);
                } else {
                    return *static_cast<U*>(this->storage_.heap_ptr);
                }
            }

            constexpr void swap(self_type& other) noexcept {
                if (this != &other) [[likely]] {
                    if (!*this && !other) [[unlikely]] {
                        return;
                    }
                    if (!*this) {
                        this->move_from(std::move(other));
                    } else if (!other) {
                        other.move_from(std::move(*this));
                    } else {
                        adaptive_function_base tmp{std::move(*this)};
                        this->move_from(std::move(other));
                        other.move_from(std::move(tmp));
                    }
                }
            }

            /**
             * @brief Destroys the callable object stored (if any).
             */
            constexpr void reset() noexcept {
                if (this->vtable_) [[likely]] {
                    this->vtable_->destroy(storage_);
                    this->vtable_ = nullptr;
                }
            }

            explicit operator bool() const noexcept {
                return vtable_ != nullptr;
            }

            /**
             * @brief Indicates whether the current callable object stored is of the given type.
             */
            template <typename F>
            requires is_callable_from_v<F>
            constexpr bool is() const noexcept {
                static_assert(concepts::decayed<F>, "F must be a decayed type");
                return vtable_ == &vtable_for<F, use_sbo_v<F>>;
            }

            /**
             * @brief Indicates whether the current callable object is stored in-place.
             */
            constexpr bool in_sbo() const noexcept {
                if (vtable_) [[likely]] {
                    return vtable_->in_sbo();
                }
                return false;
            }

            friend constexpr void swap(self_type& lhs, self_type& rhs) noexcept {
                lhs.swap(rhs);
            }

            friend constexpr bool operator==(const self_type& lhs, std::nullptr_t) noexcept {
                return lhs.vtable_ == nullptr;
            }

            friend constexpr bool operator==(std::nullptr_t, const self_type& rhs) noexcept {
                return rhs.vtable_ == nullptr;
            }
        };

    }

    template <typename T>
    inline constexpr bool is_urlicht_adaptive_function_v = functional_detail::is_urlicht_adaptive_function<T>::value;

#define ADAPTIVE_FUNCTION_SPEC(QUALIFIERS, IS_CONST, IS_LVALUE, IS_RVALUE, IS_NOEXCEPT) \
    template <typename Ret, size_t OptimizeForSize, size_t OptimizeForAlign, typename ...Args> \
    class adaptive_function<Ret(Args...) QUALIFIERS, OptimizeForSize, OptimizeForAlign> \
        : public functional_detail::adaptive_function_base< \
            Ret(Args...), OptimizeForSize, OptimizeForAlign, IS_CONST, IS_LVALUE, IS_RVALUE, IS_NOEXCEPT> { \
        using Base = functional_detail::adaptive_function_base< \
            Ret(Args...), OptimizeForSize, OptimizeForAlign, IS_CONST, IS_LVALUE, IS_RVALUE, IS_NOEXCEPT>; \
    public: \
        using Base::Base; \
        using Base::operator=; \
        using result_type = Ret; \
    };

    ADAPTIVE_FUNCTION_SPEC(, false, false, false, false)
    ADAPTIVE_FUNCTION_SPEC(const, true, false, false, false)
    ADAPTIVE_FUNCTION_SPEC(&, false, true, false, false)
    ADAPTIVE_FUNCTION_SPEC(&&, false, false, true, false)
    ADAPTIVE_FUNCTION_SPEC(noexcept, false, false, false, true)
    ADAPTIVE_FUNCTION_SPEC(const &, true, true, false, false)
    ADAPTIVE_FUNCTION_SPEC(const &&, true, false, true, false)
    ADAPTIVE_FUNCTION_SPEC(const noexcept, true, false, false, true)
    ADAPTIVE_FUNCTION_SPEC(& noexcept, false, true, false, true)
    ADAPTIVE_FUNCTION_SPEC(&& noexcept, false, false, true, true)
    ADAPTIVE_FUNCTION_SPEC(const & noexcept, true, true, false, true)
    ADAPTIVE_FUNCTION_SPEC(const && noexcept, true, false, true, true)

#undef ADAPTIVE_FUNCTION_SPEC
}

#endif //URLICHT_ADAPTIVE_FUNCTION_H
