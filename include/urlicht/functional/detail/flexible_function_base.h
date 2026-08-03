#ifndef URLICHT_FLEXIBLE_FUNCTION_BASE_H
#define URLICHT_FLEXIBLE_FUNCTION_BASE_H

#include <urlicht/config.h>
#include <urlicht/concepts/concepts.h>
#include <urlicht/internal/tag.h>
#include <urlicht/internal/scope_guard.h>
#include <utility>
#include <memory>
#include <type_traits>
#include <functional>

namespace urlicht::functional::detail {

    template <typename T, size_t Size, size_t Align, bool IsConst, bool IsLvalue,
              bool IsRvalue, bool IsNoexcept, bool Copyable, typename Alloc>
    class flexible_function_base;

    template <typename R, size_t Size, size_t Align, bool IsConst, bool IsLvalue,
              bool IsRvalue, bool IsNoexcept, bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_base<
        R(Args...), Size, Align, IsConst, IsLvalue, IsRvalue, IsNoexcept, Copyable, Alloc
    > {
    protected:
        using self_type_ = flexible_function_base;

        struct heap_storage_t_ {
            void* obj_ptr;
            void* allocated_ptr;
        };

        union storage_t_ {
            heap_storage_t_ heap;
            alignas(Align) std::byte aligned_buffer[Size];
        };

        static_assert(urlicht::concepts::allocator<Alloc>, "Alloc must be a valid allocator type.");
        using allocator_type_ = Alloc;
        using allocator_traits_ = std::allocator_traits<allocator_type_>;

        static_assert(std::same_as<typename allocator_traits_::pointer, std::byte*>,
            "The pointer of Alloc must be std::byte*");

        template <typename T>
        static constexpr bool use_sbo_v =
            sizeof(T) <= Size && alignof(T) <= Align && std::is_nothrow_move_constructible_v<T>;

        template <typename T>
        static consteval bool is_callable_from_() {
            using U = std::remove_cvref_t<T>;
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
        static constexpr bool is_callable_from_v = is_callable_from_<T>();

        template <typename T, typename ...CArgs>
        static constexpr bool nothrow_buildable_from_ =
            use_sbo_v<std::remove_cvref_t<T>> && std::is_nothrow_constructible_v<std::remove_cvref_t<T>, CArgs&&...>;

        using storage_arg_t_ = std::conditional_t<IsConst, const storage_t_&, storage_t_&>;
        using call_func_ = R(*)(storage_arg_t_, Args...) noexcept(IsNoexcept);

        struct vtable_t_ {
            call_func_ call = nullptr;
            void (*clone)(const storage_t_&, storage_t_&, allocator_type_&) = nullptr;
            void (*move)(storage_t_&, storage_t_&) noexcept = nullptr;
            void (*destroy)(storage_t_&, allocator_type_&) noexcept = nullptr;
            bool (*in_sbo)() noexcept = nullptr;
        };

        template <typename T>
        static constexpr std::size_t alloc_size_of = sizeof(T) + alignof(T) - 1;

        template <typename T>
        static constexpr heap_storage_t_ allocate_for(allocator_type_& alloc) {
            void* raw = allocator_traits_::allocate(alloc, alloc_size_of<T>);
            void* aligned = raw;
            std::size_t remaining = alloc_size_of<T>;
            std::align(alignof(T), sizeof(T), aligned, remaining);
            return {.obj_ptr = aligned, .allocated_ptr = raw};
        }

        template <typename T, bool InSBO>
        static constexpr vtable_t_ vtable_for = {
            .call = [](storage_arg_t_ storage, Args... args) noexcept(IsNoexcept) -> R {
                using CTy = std::conditional_t<IsConst, const T, T>;
                CTy* actual;
                if constexpr (InSBO) {
                    actual = reinterpret_cast<CTy*>(storage.aligned_buffer);
                } else {
                    UL_ASSERT(storage.heap.obj_ptr != nullptr, "Call on null heap pointer");
                    actual = static_cast<CTy*>(storage.heap.obj_ptr);
                }
                if constexpr (IsRvalue) {
                    return std::invoke(std::move(*actual), std::forward<Args>(args)...);
                } else {
                    return std::invoke(*actual, std::forward<Args>(args)...);
                }
            },
            .clone = [](const storage_t_& src, storage_t_& dest, [[maybe_unused]] allocator_type_& alloc) {
                if constexpr (Copyable) {
                    if constexpr (InSBO) {
                        const auto* obj = reinterpret_cast<const T*>(src.aligned_buffer);
                        std::construct_at(reinterpret_cast<T*>(dest.aligned_buffer), *obj);
                    } else {
                        auto res = allocate_for<T>(alloc);

                        auto dealloc_guard = urlicht::internal::make_scope_guard([&] {
                            allocator_traits_::deallocate(
                                alloc, static_cast<std::byte*>(res.allocated_ptr), alloc_size_of<T>
                            );
                        });
                        std::construct_at(
                            static_cast<T*>(res.obj_ptr), *static_cast<const T*>(src.heap.obj_ptr)
                        );
                        dealloc_guard.release();

                        dest.heap.allocated_ptr = res.allocated_ptr;
                        dest.heap.obj_ptr = res.obj_ptr;
                    }
                } else {
                    UL_UNREACHABLE();
                }
            },
            .move = [](storage_t_& src, storage_t_& dest) noexcept {
                if constexpr (InSBO) {
                    auto* obj = reinterpret_cast<T*>(src.aligned_buffer);
                    std::construct_at(reinterpret_cast<T*>(dest.aligned_buffer), std::move(*obj));
                    std::destroy_at(obj);
                } else {
                    dest.heap.allocated_ptr = src.heap.allocated_ptr;
                    dest.heap.obj_ptr = src.heap.obj_ptr;
                    src.heap.allocated_ptr = nullptr;
                    src.heap.obj_ptr = nullptr;
                }
            },
            .destroy = [](storage_t_& storage, [[maybe_unused]] allocator_type_& alloc) noexcept -> void {
                if constexpr (InSBO) {
                    std::destroy_at(reinterpret_cast<T*>(storage.aligned_buffer));
                } else {
                    std::destroy_at(static_cast<T*>(storage.heap.obj_ptr));
                    allocator_traits_::deallocate(
                        alloc, static_cast<std::byte*>(storage.heap.allocated_ptr), alloc_size_of<T>
                    );
                }
            },
            .in_sbo = []() noexcept {
                return InSBO;
            }
        };

        template <auto f>
        static constexpr vtable_t_ vtable_for_nontype = {
            .call = [](storage_arg_t_, Args... args) noexcept(IsNoexcept) -> R {
                return std::invoke(f, std::forward<Args>(args)...);
            },
            .clone = [](const storage_t_&, storage_t_&, allocator_type_&) { },
            .move = [](storage_t_&, storage_t_&) noexcept { },
            .destroy = [](storage_t_&, allocator_type_&) noexcept { },
            .in_sbo = [] () noexcept { return false; }
        };

        template <typename T, typename ...CArgs>
        void construct_from([[maybe_unused]] allocator_type_& alloc, CArgs&&... args)
        noexcept(nothrow_buildable_from_<T, CArgs&&...>) {
            using U = std::remove_cvref_t<T>;
            if constexpr (use_sbo_v<U>) {
                std::construct_at(
                    reinterpret_cast<U*>(storage_.aligned_buffer),
                    std::forward<CArgs>(args)...
                );
            } else {
                auto res = allocate_for<U>(alloc);

                auto dealloc_guard = urlicht::internal::make_scope_guard([&] {
                    allocator_traits_::deallocate(
                        alloc, static_cast<std::byte*>(res.allocated_ptr), alloc_size_of<U>
                    );
                });
                std::construct_at(static_cast<U*>(res.obj_ptr), std::forward<CArgs>(args)...);
                dealloc_guard.release();

                storage_.heap.allocated_ptr = res.allocated_ptr;
                storage_.heap.obj_ptr = res.obj_ptr;
            }
            vtable_ = &vtable_for<U, use_sbo_v<U>>;
        }

        // IMPORTANT: Allocator should be correctly handled before calling copy_from and move_from
        void copy_from(const flexible_function_base& other)
        requires Copyable {
            other.vtable_->clone(other.storage_, this->storage_, this->alloc_);
            this->vtable_ = other.vtable_;
        }

        void move_from(flexible_function_base&& other) noexcept {
            other.vtable_->move(other.storage_, this->storage_);
            this->vtable_ = other.vtable_;
            other.vtable_ = nullptr;
        }

        // Data members
        storage_t_ storage_
#if UL_HAS_CPP26
[[indeterminate]]
#endif
        ;
        const vtable_t_* vtable_{};
        UL_NO_UNIQUE_ADDRESS allocator_type_ alloc_{};

    public:
        // Constructors
        constexpr flexible_function_base() noexcept = default;
        constexpr flexible_function_base(std::nullptr_t) noexcept { }

        template <urlicht::concepts::can_construct<allocator_type_> Alloc_>
        constexpr explicit flexible_function_base(const Alloc_& alloc)
        noexcept(std::is_nothrow_constructible_v<allocator_type_, const Alloc_&>)
        : alloc_{alloc} { }

        /**
         * @brief Constructs from the given callable object.
         * @param func The callable to store. Requirements:
         *        1. It must be compatible with the specifiers.
         *        2. It must not be of the same specialization of flexible_function.
         *        3. If RequireCopy == true, it must be copy-constructible.
         */
        template <typename F>
        requires is_callable_from_v<F> &&
                 (!std::is_base_of_v<self_type_, std::remove_cvref_t<F>>) &&
                 std::constructible_from<std::remove_cvref_t<F>, F&&>
        constexpr flexible_function_base(F&& func)
        noexcept(nothrow_buildable_from_<F, F&&>) {
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<std::remove_cvref_t<F>>, "F must be copyable");
            }
            if constexpr (requires() { func == nullptr; }) {
                if (func == nullptr) [[unlikely]] {
                    return;
                }
            }
            construct_from<F>(alloc_, std::forward<F>(func));
        }

        template <typename F, urlicht::concepts::can_construct<allocator_type_> Alloc_>
        requires is_callable_from_v<F> &&
                 (!std::is_base_of_v<self_type_, std::remove_cvref_t<F>>) &&
                 std::constructible_from<std::remove_cvref_t<F>, F&&>
        constexpr flexible_function_base(urlicht::internal::allocator_arg_t, const Alloc_& alloc, F&& func)
        noexcept(nothrow_buildable_from_<F, F&&> &&
                 std::is_nothrow_constructible_v<allocator_type_, const Alloc&>)
        : alloc_{alloc} {
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<std::remove_cvref_t<F>>, "F must be copyable");
            }
            if constexpr (requires() { func == nullptr; }) {
                if (func == nullptr) [[unlikely]] {
                    return;
                }
            }
            construct_from<F>(alloc_, std::forward<F>(func));
        }

        /**
         * @brief Constructs the designated type in-place from the given arguments.
         */
        template <typename F, typename... _Args>
        requires is_callable_from_v<F> &&
                 (!std::is_base_of_v<self_type_, F>) &&
                 std::constructible_from<F, _Args&&...>
        constexpr flexible_function_base(urlicht::internal::inplace_t<F>, _Args&&... args)
        noexcept(nothrow_buildable_from_<F, _Args&&...>) {
            static_assert(urlicht::concepts::decayed<F>, "F must be a decayed type");
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<F>, "F must be copyable");
            }
            construct_from<F>(alloc_, std::forward<_Args>(args)...);
        }

        template <urlicht::concepts::can_construct<allocator_type_> Alloc_,
                  typename F, typename... _Args>
        requires is_callable_from_v<F> &&
                 (!std::is_base_of_v<self_type_, F>) &&
                 std::constructible_from<F, _Args&&...>
        constexpr flexible_function_base(
            urlicht::internal::allocator_arg_t, const Alloc_& alloc, urlicht::internal::inplace_t<F>, _Args&&... args
        ) noexcept(nothrow_buildable_from_<F, _Args&&...> &&
                   std::is_nothrow_constructible_v<allocator_type_, const Alloc&>)
        : alloc_{alloc} {
            static_assert(urlicht::concepts::decayed<F>, "F must be a decayed type");
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<F>, "F must be copyable");
            }
            construct_from<F>(alloc_, std::forward<_Args>(args)...);
        }

        /**
         * @brief Constructs the designated type in-place from an initializer-list and other given arguments.
         */
        template <typename F, typename C, typename ..._Args>
        requires is_callable_from_v<F> &&
                 (!std::is_base_of_v<self_type_, F>) &&
                 std::constructible_from<F, std::initializer_list<C>, _Args&&...>
        constexpr explicit flexible_function_base(
            urlicht::internal::inplace_t<F>, std::initializer_list<C> il, _Args&&... args
        ) noexcept(nothrow_buildable_from_<F, std::initializer_list<C>, _Args&&...>) {
            static_assert(urlicht::concepts::decayed<F>, "F must be a decayed type");
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<F>, "F must be copyable");
            }
            construct_from<F>(alloc_, il, std::forward<_Args>(args)...);
        }

        template <urlicht::concepts::can_construct<allocator_type_> Alloc_,
                  typename F, typename C, typename ..._Args>
        requires is_callable_from_v<F> &&
                 (!std::is_base_of_v<self_type_, F>) &&
                 std::constructible_from<F, std::initializer_list<C>, _Args&&...>
        constexpr explicit flexible_function_base(
            urlicht::internal::allocator_arg_t, const Alloc_& alloc,
            urlicht::internal::inplace_t<F>, std::initializer_list<C> il, _Args&&... args
        ) noexcept(nothrow_buildable_from_<F, std::initializer_list<C>, _Args&&...> &&
                   std::is_nothrow_constructible_v<allocator_type_, const Alloc&>)
        : alloc_{alloc} {
            static_assert(urlicht::concepts::decayed<F>, "F must be a decayed type");
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<F>, "F must be copyable");
            }
            construct_from<F>(alloc_, il, std::forward<_Args>(args)...);
        }

        /**
         * @brief Constructs with a NTTP wrapped by {nontype}.
         *        Example usage: flexible_function func{nontype<func_ptr>};
         */
        template <auto f>
        requires is_callable_from_v<decltype(f)>
        constexpr flexible_function_base(urlicht::internal::nontype_t<f>) noexcept {
            this->vtable_ = &vtable_for_nontype<f>;
        }

        template <urlicht::concepts::can_construct<allocator_type_> Alloc_, auto f>
        requires is_callable_from_v<decltype(f)>
        constexpr flexible_function_base(
            urlicht::internal::allocator_arg_t, const Alloc_& alloc, urlicht::internal::nontype_t<f>
        ) noexcept(std::is_nothrow_constructible_v<allocator_type_, const Alloc&>)
        : alloc_{alloc} {
            this->vtable_ = &vtable_for_nontype<f>;
        }

        constexpr flexible_function_base(const flexible_function_base& other)
        requires (Copyable)
        : alloc_{allocator_traits_::select_on_container_copy_construction(other.alloc_)} {
            if (other) [[likely]] {
                copy_from(other);
            }
        }

        constexpr flexible_function_base(const flexible_function_base& other)
        requires (!Copyable) = delete;

        constexpr flexible_function_base(flexible_function_base&& other)
        noexcept(std::is_nothrow_move_constructible_v<allocator_type_>)
        : alloc_{std::move(other.alloc_)} {
            if (other) [[likely]] {
                move_from(std::move(other));
            }
        }

        constexpr flexible_function_base& operator=(const flexible_function_base& other)
        requires (Copyable) {
            if (this != &other) [[likely]] {
                this->reset();
                // Even if {other} is empty, we may still want to copy its allocator
                if constexpr (allocator_traits_::propagate_on_container_copy_assignment::value) {
                    this->alloc_ = other.alloc_;
                }
                if (other) [[likely]] {
                    copy_from(other);
                }
            }
            return *this;
        }

        /**
         * @note: This falls back to copy assignment if:
         *        1. allocator_type does not propagate on move assignment, AND
         *        2. Instances of allocator_type are not always equal, AND
         *        3. The src and dest allocators are not equal.
         */
        constexpr flexible_function_base& operator=(flexible_function_base&& other)
        noexcept(
            allocator_traits_::propagate_on_container_move_assignment::value
            ? std::is_nothrow_move_assignable_v<allocator_type_>
            : allocator_traits_::is_always_equal::value
        ) {
            if (this != &other) [[likely]] {
                this->reset();
                if constexpr (allocator_traits_::propagate_on_container_move_assignment::value) {
                    this->alloc_ = std::move(other.alloc_);
                } else if constexpr (allocator_traits_::is_always_equal::value) {
                    // pass
                } else if (this->alloc_ == other.alloc_) {
                    // pass
                } else { // Resorts to copy assignment
                    if (other) [[likely]] {
                        copy_from(other);
                        other.reset(); // For consistent behavior
                    }
                    return *this;
                }
                if (other) [[likely]] {
                    move_from(std::move(other));
                }
            }
            return *this;
        }

        constexpr flexible_function_base& operator=(std::nullptr_t) noexcept {
            this->reset();
            return *this;
        }

        template <typename F>
        requires is_callable_from_v<F> &&
                 std::constructible_from<std::remove_cvref_t<F>, F&&> &&
                 (!std::is_base_of_v<self_type_, std::remove_cvref_t<F>>)
        constexpr flexible_function_base& operator=(F&& func) {
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<std::remove_cvref_t<F>>, "F must be copyable");
            }
            this->reset();
            if constexpr (requires() { func == nullptr; }) {
                if (func == nullptr) [[unlikely]] {
                    return *this;
                }
            }
            construct_from<F>(alloc_, std::forward<F>(func));
            return *this;
        }

        constexpr ~flexible_function_base() noexcept {
            this->reset();
        }

        template <typename F, typename ... CArgs>
        requires std::constructible_from<std::remove_cvref_t<F>, CArgs&&...> &&
                 is_callable_from_v<F>
        constexpr std::remove_cvref_t<F>& emplace(CArgs&& ...cargs) {
            using U = std::remove_cvref_t<F>;
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<U>, "F must be copyable");
            }
            this->reset();
            construct_from<U>(alloc_, std::forward<CArgs>(cargs)...);

            if constexpr (use_sbo_v<U>) {
                return *reinterpret_cast<U*>(this->storage_.aligned_buffer);
            } else {
                return *static_cast<U*>(this->storage_.heap.obj_ptr);
            }
        }

        template <typename F, typename T, typename ... CArgs>
        requires std::constructible_from<std::remove_cvref_t<F>, std::initializer_list<T>, CArgs&&...> &&
                 is_callable_from_v<F>
        constexpr std::remove_cvref_t<F>& emplace(std::initializer_list<T> il, CArgs&& ...cargs) {
            using U = std::remove_cvref_t<F>;
            if constexpr (Copyable) {
                static_assert(std::copy_constructible<U>, "F must be copyable");
            }
            this->reset();
            construct_from<U>(alloc_, il, std::forward<CArgs>(cargs)...);

            if constexpr (use_sbo_v<U>) {
                return *reinterpret_cast<U*>(storage_.aligned_buffer);
            } else {
                return *static_cast<U*>(storage_.heap.obj_ptr);
            }
        }

        /**
         * @warning If allocator_type does not propagate on swap, then the allocators from source and dest
         *          must compare equal. Otherwise, the behavior is undefined.
         */
        constexpr void swap(self_type_& other)
        noexcept(std::is_nothrow_swappable_v<allocator_type_>) {
            if (this != &other) [[likely]] {
                if constexpr (allocator_traits_::propagate_on_container_swap::value) {
                    using std::swap;
                    swap(this->alloc_, other.alloc_);
                } else {
                    if constexpr (!allocator_traits_::is_always_equal::value) {
                        UL_ASSERT(
                            this->alloc_ == other.alloc_,
                            "Cannot swap flexible_function objects with unequal non-propagating allocators"
                        );
                    }
                }
                storage_t_ tmp_storage{};
                const vtable_t_* tmp_vtable = this->vtable_;

                if (this->vtable_) {
                    this->vtable_->move(this->storage_, tmp_storage);
                }
                if (other.vtable_) {
                    other.vtable_->move(other.storage_, this->storage_);
                }
                if (tmp_vtable) {
                    tmp_vtable->move(tmp_storage, other.storage_);
                }
                this->vtable_ = other.vtable_;
                other.vtable_ = tmp_vtable;
            }
        }

        /**
         * @brief Destroys the callable object stored (if any).
         */
        constexpr void reset() noexcept {
            if (vtable_) [[likely]] {
                vtable_->destroy(storage_, alloc_);
                vtable_ = nullptr;
            }
        }

        explicit constexpr operator bool() const noexcept {
            return vtable_ != nullptr;
        }

        /**
         * @brief Indicates whether the current callable object stored is of the given type.
         */
        template <typename F>
        requires is_callable_from_v<F>
        constexpr bool is() const noexcept {
            static_assert(urlicht::concepts::decayed<F>, "F must be a decayed type");
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

        constexpr allocator_type_ get_allocator() const {
            return this->alloc_;
        }

        friend constexpr void swap(self_type_& lhs, self_type_& rhs)
        noexcept(noexcept(lhs.swap(rhs))) {
            lhs.swap(rhs);
        }

        friend constexpr bool operator==(const self_type_& lhs, std::nullptr_t) noexcept {
            return lhs.vtable_ == nullptr;
        }

        friend constexpr bool operator==(std::nullptr_t, const self_type_& rhs) noexcept {
            return rhs.vtable_ == nullptr;
        }
    };

    //*************************** Decorator (Mixins) ***************************//

    // We could have put all the function call operators with different specifiers in the base class,
    // and ensured only one of them is instantiated by mutually exclusive require clauses. However, clangd
    // is often not aware that only one is viable, thus giving a warning of "Cannot overload a member
    // function with ref-qualifier '&' with a member function without a ref-qualifier". Therefore, it is
    // necessary to separate them in mixin classes.

    template <typename T, size_t Size, size_t Align, bool IsConst, bool IsLvalue,
              bool IsRvalue, bool Noexcept, bool Copyable, typename Alloc>
    class flexible_function_call_operator_;

    template <typename R, size_t Size, size_t Align, bool Noexcept,
              bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_call_operator_<R(Args...), Size, Align, false, false, false, Noexcept, Copyable, Alloc>
    : public flexible_function_base<R(Args...), Size, Align, false, false, false, Noexcept, Copyable, Alloc> {
        using base_t =
            flexible_function_base<R(Args...), Size, Align, false, false, false, Noexcept, Copyable, Alloc>;
    public:
        using base_t::base_t;
        using base_t::operator=;

        R operator()(Args... args) noexcept(Noexcept) {
            UL_ASSERT(this->vtable_ != nullptr, "flexible_function is null");
            return this->vtable_->call(this->storage_, std::forward<Args>(args)...);
        }
    };

    template <typename R, size_t Size, size_t Align, bool Noexcept,
              bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_call_operator_<R(Args...), Size, Align, true, false, false, Noexcept, Copyable, Alloc>
    : public flexible_function_base<R(Args...), Size, Align, true, false, false, Noexcept, Copyable, Alloc> {
        using base_t =
            flexible_function_base<R(Args...), Size, Align, true, false, false, Noexcept, Copyable, Alloc>;
    public:
        using base_t::base_t;
        using base_t::operator=;

        R operator()(Args... args) const noexcept(Noexcept) {
            UL_ASSERT(this->vtable_ != nullptr, "flexible_function is null");
            return this->vtable_->call(this->storage_, std::forward<Args>(args)...);
        }
    };

    template <typename R, size_t Size, size_t Align, bool Noexcept,
              bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_call_operator_<R(Args...), Size, Align, false, true, false, Noexcept, Copyable, Alloc>
    : public flexible_function_base<R(Args...), Size, Align, false, true, false, Noexcept, Copyable, Alloc> {
        using base_t =
            flexible_function_base<R(Args...), Size, Align, false, true, false, Noexcept, Copyable, Alloc>;
    public:
        using base_t::base_t;
        using base_t::operator=;

        R operator()(Args... args) & noexcept(Noexcept) {
            UL_ASSERT(this->vtable_ != nullptr, "flexible_function is null");
            return this->vtable_->call(this->storage_, std::forward<Args>(args)...);
        }
    };

    template <typename R, size_t Size, size_t Align, bool Noexcept,
              bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_call_operator_<R(Args...), Size, Align, false, false, true, Noexcept, Copyable, Alloc>
    : public flexible_function_base<R(Args...), Size, Align, false, false, true, Noexcept, Copyable, Alloc> {
        using base_t =
            flexible_function_base<R(Args...), Size, Align, false, false, true, Noexcept, Copyable, Alloc>;
    public:
        using base_t::base_t;
        using base_t::operator=;

        R operator()(Args... args) && noexcept(Noexcept) {
            UL_ASSERT(this->vtable_ != nullptr, "flexible_function is null");
            return this->vtable_->call(this->storage_, std::forward<Args>(args)...);
        }
    };

    template <typename R, size_t Size, size_t Align, bool Noexcept,
              bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_call_operator_<R(Args...), Size, Align, true, true, false, Noexcept, Copyable, Alloc>
    : public flexible_function_base<R(Args...), Size, Align, true, true, false, Noexcept, Copyable, Alloc> {
        using base_t =
            flexible_function_base<R(Args...), Size, Align, true, true, false, Noexcept, Copyable, Alloc>;
    public:
        using base_t::base_t;
        using base_t::operator=;

        R operator()(Args... args) const & noexcept(Noexcept) {
            UL_ASSERT(this->vtable_ != nullptr, "flexible_function is null");
            return this->vtable_->call(this->storage_, std::forward<Args>(args)...);
        }
    };

    template <typename R, size_t Size, size_t Align, bool Noexcept,
              bool Copyable, typename Alloc, typename ...Args>
    class flexible_function_call_operator_<R(Args...), Size, Align, true, false, true, Noexcept, Copyable, Alloc>
    : public flexible_function_base<R(Args...), Size, Align, true, false, true, Noexcept, Copyable, Alloc> {
        using base_t =
            flexible_function_base<R(Args...), Size, Align, true, false, true, Noexcept, Copyable, Alloc>;
    public:
        using base_t::base_t;
        using base_t::operator=;

        R operator()(Args... args) const && noexcept(Noexcept) {
            UL_ASSERT(this->vtable_ != nullptr, "flexible_function is null");
            return this->vtable_->call(this->storage_, std::forward<Args>(args)...);
        }
    };
}

#endif //URLICHT_FLEXIBLE_FUNCTION_BASE_H
