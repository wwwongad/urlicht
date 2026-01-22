#ifndef URLICHT_INPLACE_VECTOR_H
#define URLICHT_INPLACE_VECTOR_H
#pragma once
#include <initializer_list>
#include <utility>
#include <iterator>
#include <algorithm>
#include <stdexcept>
#include <new>
#include <compare>
#include <cstdint>
#include <cstring>
#include <limits>
#include <concepts>
#include <type_traits>
#include <urlicht/concepts_utility.h>

namespace urlicht {

    /**
     * @brief A dynamically resizable array with fixed (compile-time constant) capacity. Internal storage is
     *        allocated on the stack, thus inplace, and is contiguous. Elements are stored within the
     *        inplace_vector with proper alignment. Upon C++26, all member functions will be able to evaluate
     *        at compile time, provided that T is constexpr.
     *
     * @tparam T Any type that satisfies std::is_object_v.
     * @tparam N The maximum number of elements in the inplace_vector
     *
     * @warning: If N is large, you may want to use a heap-allocated vector (e.g. std::vector) to avoid stack overflow.
     */
    template <concepts::object T, std::size_t N>
    class inplace_vector;

    namespace detail {

        template <std::size_t N>
        using adaptive_size_type =
            std::conditional_t<N <= std::numeric_limits<uint8_t>::max(), uint8_t,
                std::conditional_t<N <= std::numeric_limits<uint16_t>::max(), uint16_t,
                    std::conditional_t<N <= std::numeric_limits<uint32_t>::max(), uint32_t,
                        std::conditional_t<N <= std::numeric_limits<uint64_t>::max(), uint64_t, std::size_t>
                    >
                >
            >;

        template <std::ranges::range Range>
        constexpr auto range_begin(Range&& rng) noexcept {
            if constexpr (concepts::rvalue_range<Range>) {
                return std::make_move_iterator(std::ranges::begin(rng));
            } else {
                return std::ranges::begin(rng);
            }
        }

        template <std::ranges::range Range>
        constexpr auto range_end(Range&& rng) noexcept {
            if constexpr (concepts::rvalue_range<Range>) {
                return std::move_sentinel(std::ranges::end(rng));
            } else {
                return std::ranges::end(rng);
            }
        }

        template <typename T, std::size_t N>
        struct unified_storage {
            using size_type = adaptive_size_type<N>;
            using storage_type = std::byte[N * sizeof(T)];

        private:
            alignas(alignof(T)) storage_type storage_;  // Deliberately not initialized
            size_type size_{0U};

        public:
            constexpr unified_storage() = default;

            // Copy constructor
            constexpr unified_storage(const unified_storage& other)
            noexcept(std::is_nothrow_copy_constructible_v<T>) {
                append_with_size(other.data(), other.size());
            }

            // Move constructor
            constexpr unified_storage(unified_storage&& other)
            noexcept (std::is_nothrow_move_constructible_v<T>) {
                append_with_size(std::make_move_iterator(other.data()), other.size());
                other.clear();
            }

            // Size constructor
            constexpr unified_storage(const size_type new_size) {
                if (new_size > static_cast<size_type>(N)) [[unlikely]] {
                    throw std::bad_alloc();
                }
                std::uninitialized_value_construct_n(this->data(), new_size);
                size_ = new_size;
            }

            // Valued size constructor
            constexpr unified_storage(const size_type new_size, const T& value) {
                if (new_size > static_cast<size_type>(N)) [[unlikely]] {
                    throw std::bad_alloc();
                }
                fill_with_size(new_size, value);
            }

            // Iterator constructor
            template <concepts::compatible_iterator<T> Iter, std::sentinel_for<Iter> Sentinel>
            constexpr unified_storage(Iter first, Sentinel last) {
                if constexpr (std::forward_iterator<Iter>) {
                    const auto size = std::ranges::distance(first, last);
                    if (size > static_cast<size_type>(N)) [[unlikely]] {
                        throw std::bad_alloc();
                    }
                    append_with_size(first, size);
                } else {
                    auto [until, size] = append_with_sentinel(std::move(first), last);
                    if (until != last) [[unlikely]] {
                        throw std::bad_alloc();
                    }
                }
            }

            // Range constructor
            template <concepts::compatible_range<T> Rng>
            constexpr explicit unified_storage(Rng&& rng)
            : unified_storage(range_begin(std::forward<Rng>(rng)),
                              range_end(std::forward<Rng>(rng))) {}

            // Initializer list constructor
            template <typename VTy>
            requires std::constructible_from<T, VTy>
            constexpr unified_storage(std::initializer_list<VTy> list)
            : unified_storage(list.begin(), list.end()) { }


            // Copy assignment operator
            constexpr unified_storage& operator=(const unified_storage& other)
            noexcept(std::is_nothrow_copy_assignable_v<T> &&
                     std::is_nothrow_copy_constructible_v<T>) {
                if (this == &other) [[unlikely]] {
                    return *this;
                }
                assign_with_size(other.data(), other.size());
                return *this;
            }

            // Move assignment operator
            constexpr unified_storage& operator=(unified_storage&& other)
            noexcept(std::is_nothrow_move_assignable_v<T> &&
                     std::is_nothrow_move_constructible_v<T>) {
                if (this == &other) [[unlikely]] {
                    return *this;
                }
                assign_with_size(std::make_move_iterator(other.data()), other.size_);
                other.clear();
                return *this;
            }

            template <concepts::compatible_iterator<T> Iter>
            constexpr void assign_with_size(Iter first, const size_type n)
            noexcept(std::is_nothrow_constructible_v<T, std::iter_reference_t<Iter>> &&
                     std::is_nothrow_assignable_v<T, std::iter_reference_t<Iter>>) {
                const auto old_size = size_;
                const auto k = std::min(n, old_size);
                auto res = std::ranges::copy_n(first, k, this->data());
                if (n > old_size) {
                    std::uninitialized_copy_n(res.in, n - old_size, this->data() + old_size);
                } else if (n < old_size) {
                    std::destroy_n(this->data() + n, old_size - n);
                }
                size_ = n;
            }

            template <concepts::compatible_iterator<T> Iter, std::sentinel_for<Iter> Sent>
            constexpr auto assign_with_sentinel(Iter first, Sent sentinel)
            noexcept(std::is_nothrow_constructible_v<T, std::iter_reference_t<Iter>> &&
                     std::is_nothrow_assignable_v<T, std::iter_reference_t<Iter>>) {
                // Because this has two phrases, strong exception guarantee is impossible without
                // creating extra copies
                const auto old_size = size_;
                size_type cnt{};
                for (; first != sentinel && cnt != old_size; ++first, ++cnt) {
                    this->data()[cnt] = *first;
                }
                if (cnt != old_size) {
                    std::destroy_n(this->data() + cnt, old_size - cnt);
                } else if (first != sentinel) {
                    for (; first != sentinel && cnt != N; ++first, ++cnt) {
                        std::construct_at(this->data() + cnt, *first);
                    }
                }
                size_ = cnt;
                return std::make_pair(std::move(first), cnt);
            }

            template <concepts::compatible_iterator<T> Iter>
            constexpr void append_with_size(Iter first, const size_type n)
            noexcept(std::is_nothrow_constructible_v<T, std::iter_reference_t<Iter>>) {
                const auto old_size = size_;
                if constexpr (std::is_trivially_copyable_v<T> && std::contiguous_iterator<Iter> &&
                              std::same_as<std::iter_value_t<Iter>, T>) {
                    std::memmove(this->data() + old_size, std::to_address(first), n * sizeof(T));
                } else {
                    std::uninitialized_copy_n(first, n, this->data() + old_size);
                }
                size_ += n;
            }

            template <concepts::compatible_iterator<T> Iter, std::sentinel_for<Iter> Sent>
            constexpr auto append_with_sentinel(Iter first, Sent last)
            noexcept(std::is_nothrow_constructible_v<T, std::iter_reference_t<Iter>>) {
                size_type cnt{};
                if constexpr (std::is_nothrow_constructible_v<T, std::iter_reference_t<Iter>>) {
                    for (; first != last && size_ != N; ++first, ++size_, ++cnt) {
                        std::construct_at(this->data() + size_, *first);
                    }
                    return std::make_pair(std::move(first), cnt);
                } else {
                    const auto old_size = size_;
                    try {
                        for (; first != last && size_ != N; ++first, ++size_, ++cnt) {
                            std::construct_at(this->data() + size_, *first);
                        }
                        return std::make_pair(std::move(first), cnt);
                    } catch (...) {
                        std::destroy_n(this->data() + old_size, cnt);
                        size_ = old_size;
                        throw;
                    }
                }
            }

            constexpr void fill_with_size(const size_type n, const T& val)
            noexcept(std::is_nothrow_copy_constructible_v<T>) {
                if constexpr (sizeof(T) == 1 && std::is_trivially_copy_constructible_v<T>) {
                    std::memset(this->data() + size_, val, n * sizeof(T));
                } else {
                    std::uninitialized_fill_n(this->data() + size_, n, val);
                }
                size_ += n;
            }

            // Destructor
            constexpr ~unified_storage() noexcept(std::is_nothrow_destructible_v<T>) {
                if constexpr (!std::is_trivially_destructible_v<T>) {
                    std::destroy_n(this->data(), size_);
                }
            }

            [[nodiscard]] constexpr T* data() noexcept
            requires (!std::is_const_v<T>) {
                return reinterpret_cast<T*>(std::addressof(storage_));
            }

            [[nodiscard]] constexpr const T* data() const noexcept {
                return reinterpret_cast<const T*>(std::addressof(storage_));
            }

            [[nodiscard]] constexpr size_type size() const noexcept {
                return size_;
            }

            constexpr void unchecked_set_size(size_type new_size) noexcept {
                size_ = new_size;
            }

            constexpr void clear() noexcept(std::is_nothrow_destructible_v<T>) {
                if constexpr (!std::is_trivially_destructible_v<T>) {
                    std::destroy_n(this->data(), size_);
                }
                size_ = 0U;
            }

        };

    } // namespace detail

    /************************* Identity *************************/

    namespace detail {
        template <typename Other>
        struct is_inplace_vector : std::false_type {};

        template <typename T, std::size_t N>
        struct is_inplace_vector<inplace_vector<T, N>> : std::true_type {};
    }

    template <typename Cont>
    inline constexpr bool is_inplace_vector_v = detail::is_inplace_vector<Cont>::value;

    template <concepts::object T, size_t N>
    class inplace_vector {
        static_assert(N > 0, "N must be greater than zero");
        using storage_type = detail::unified_storage<T, N>;
    public:
        using value_type = T;
        using size_type = typename storage_type::size_type;
        using difference_type = std::ptrdiff_t;
        using reference = T&;
        using const_reference = const T&;
        using pointer = T*;
        using const_pointer = const T*;
        using iterator = pointer;
        using const_iterator = const_pointer;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    private:
        [[no_unique_address]] storage_type storage_;

    public:
        /****************************** CONSTRUCTORS ******************************/

        // Default constructor -- initializes size only
        constexpr inplace_vector() noexcept = default;

        // Copy constructor
        constexpr inplace_vector(const inplace_vector& other)
        noexcept(std::is_nothrow_copy_constructible_v<storage_type>)
        requires std::copy_constructible<value_type> = default;

        // Move constructor
        constexpr inplace_vector(inplace_vector&& other)
        noexcept(std::is_nothrow_move_constructible_v<storage_type>)
        requires std::move_constructible<value_type> = default;

        // Size constructor - default initializes the first {size} elements
        constexpr explicit inplace_vector(const size_type size)
        requires std::default_initializable<value_type>
        : storage_(size) { }

        // Valued size constructor - fill the first {size} elements with {value}
        constexpr inplace_vector(const size_type size, const_reference value)
        requires std::copy_constructible<value_type>
        : storage_(size, value) { }

        // Iterator constructor
        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        constexpr inplace_vector(Iter first, Sentinel last)
        : storage_(std::move(first), last) { }

        // Range constructor
        template <concepts::compatible_range<T> Rng>
        requires (!is_inplace_vector_v<std::remove_cvref_t<Rng>>)
        constexpr explicit inplace_vector(Rng&& rng)
        : storage_(std::forward<Rng>(rng)) { }

        // Initializer list constructor
        template <concepts::can_construct<value_type> VTy>
        constexpr inplace_vector(std::initializer_list<VTy> init)
        : storage_(init) { }

        // Destructor
        constexpr ~inplace_vector()
        noexcept(std::is_nothrow_destructible_v<storage_type>) = default;

        // Assignment operators
        // Copy assignment operator
        constexpr inplace_vector& operator=(const inplace_vector& other)
        noexcept(std::is_nothrow_copy_assignable_v<storage_type>)
        requires std::is_copy_assignable_v<value_type> = default;

        // Move assignment operator
        constexpr inplace_vector& operator=(inplace_vector&& other)
        noexcept(std::is_nothrow_move_assignable_v<storage_type>)
        requires std::is_move_assignable_v<value_type> = default;

        //////////////////////////////////////////////
        //                assign_*                  //
        //////////////////////////////////////////////

        /**
         * @brief Replaces the content of the vector with [count] copies of [value].
         *
         * @param count: number of elements to assign.
         * @param value: the value to copy into the vector.
         *
         * @throws std::bad_alloc if count > N. No changes to the vector will be made.
         * @throws _ Any exception thrown during the copy construction or assignment of value_type.
         *           Only basic exception guarantee is provided in this case.
         */
        constexpr void assign(size_type count, const value_type& value)
        requires std::assignable_from<value_type&, const value_type&> &&
                 std::copy_constructible<value_type> {
            if (count > max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            const auto old_size = this->size();
            const auto k = std::min(count, old_size);

            std::fill_n(this->data(), k, value);
            if (count > old_size) {
                this->storage_.fill_with_size(count - old_size, value);
            } else {
                std::destroy_n(this->begin() + count, old_size - count);
                this->storage_.unchecked_set_size(count);
            }
        }

        /**
         * @brief Replaces the content of the vector with [count] default-initialized elements.
         *
         * @param count: number of elements to assign.
         *
         * @throws std::bad_alloc if count > N. No changes to the vector will be made.
         * @throws _ Any exception thrown during default construction or assignment of value_type.
         *           Only basic exception guarantee is provided in this case.
         *
         * @note: value_type must be default initializable
         */
        constexpr void assign(size_type count)
        requires std::default_initializable<value_type> {
            this->assign(count, value_type{});
        }

        /**
         * @brief Replaces the content of the vector with elements in [first, last).
         *
         * @tparam Iter: input iterator or above. value_type must be constructible and
         *               assignable from iter_reference_t<Iter>.
         * @tparam Sentinel: sentinel for Iter
         *
         * @throws   std::bad_alloc if distance(first, last) > N. No changes to the vector
         *           will be made.
         * @throws   _ Any exception thrown during the assignment and construction of value_type.
         *           Basic exception guarantee only.
         */
        template <concepts::compatible_iterator<value_type> Iter, std::sentinel_for<Iter> Sentinel>
        constexpr void assign(Iter first, Sentinel last)
        requires std::assignable_from<value_type&, std::iter_reference_t<Iter>> {
            if constexpr (std::forward_iterator<Iter>) {
                const auto cnt = static_cast<size_type>(std::ranges::distance(first, last));
                if (cnt > max_size()) [[unlikely]] {
                    throw std::bad_alloc{};
                }
                this->storage_.assign_with_size(first, cnt);
            } else {
                inplace_vector temp(std::move(first), last);
                *this = std::move(temp);
            }
        }

        /**
         * @brief Replaces the content of the vector with the elements from the initializer list.
         *
         * @tparam VTy: type of elements in the initializer list, from which value_type must be
         *              constructible and assignable
         *
         * @throws std::bad_alloc if il.size() > N. No changes to the vector will be made.
         * @throws _ Any exception thrown during the assignment or construction of value_type.
         *           Basic exception guarantee.
         */
        template <typename VTy>
        constexpr void assign(std::initializer_list<VTy> il)
        requires std::constructible_from<value_type, VTy> &&
                 std::assignable_from<value_type&, VTy> {
            this->assign(il.begin(), il.end());
        }

        /**
         * @brief Replaces the content of the vector with the elements from the given range.
         *
         * @tparam Rng: input_range or above. value_type must be constructible and assignable
         *              from reference_t<Rng&&>
         *
         * @throws std::bad_alloc if size(rng) > N. No changes to the vector will be made.
         * @throws _ Any exception thrown during the assignment or construction of value_type.
         *           Basic exception guarantee.
         *
         * @note: If Rng&& is a rvalue range, its elements are moved from it.
         */
        template <concepts::compatible_range<value_type> Rng>
        constexpr void assign_range(Rng&& rng)
        requires std::assignable_from<value_type&, std::ranges::range_reference_t<Rng&&>> {
            this->assign(detail::range_begin(std::forward<Rng>(rng)),
                         detail::range_end(std::forward<Rng>(rng)));
        }

        /****************************** ELEMENT ACCESS ******************************/

        [[nodiscard]] constexpr pointer data() noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data();
        }

        [[nodiscard]] constexpr const_pointer data() const noexcept {
            return storage_.data();
        }

        [[nodiscard]] constexpr reference front() noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data()[0];
        }

        [[nodiscard]] constexpr const_reference front() const noexcept {
            return storage_.data()[0];
        }

        [[nodiscard]] constexpr reference back() noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data()[storage_.size() - 1];
        }

        [[nodiscard]] constexpr const_reference back() const noexcept {
            return storage_.data()[storage_.size() - 1];
        }

        [[nodiscard]] constexpr reference operator[](const size_type index) noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data()[index];
        }

        [[nodiscard]] constexpr const_reference operator[](const size_type index) const noexcept {
            return storage_.data()[index];
        }

        [[nodiscard]] constexpr reference at(const size_type index)
        requires (!std::is_const_v<value_type>) {
            if (index >= storage_.size()) [[unlikely]] {
                throw std::out_of_range("inplace_vector::at");
            }
            return storage_.data()[index];
        }

        [[nodiscard]] constexpr const_reference at(const size_type index) const {
            if (index >= storage_.size()) [[unlikely]] {
                throw std::out_of_range("inplace_vector::at");
            }
            return storage_.data()[index];
        }

        /****************************** CAPACITY ******************************/

        [[nodiscard]] constexpr size_type size() const noexcept {
            return storage_.size();
        }

        [[nodiscard]] constexpr std::size_t ssize() const noexcept {
            return static_cast<std::size_t>(this->size());
        }

        [[nodiscard]] constexpr bool empty() const noexcept {
            return this->size() == 0U;
        }

        [[nodiscard]] constexpr bool is_full() const noexcept {
            return size() == max_size();
        }

        [[nodiscard]] static consteval size_type capacity() noexcept {
            return N;
        }

        [[nodiscard]] static consteval size_type max_size() noexcept {
            return N;
        }

        static constexpr void reserve(size_type size) {
            if (size > N) [[unlikely]] {
                throw std::bad_alloc{};
            }
        }

        static consteval void shrink_to_fit() noexcept { /* nop */ }

        /****************************** ITERATORS ******************************/

        [[nodiscard]] constexpr iterator begin() noexcept
        requires (!std::is_const_v<value_type>) {
            return storage_.data();
        }

        [[nodiscard]] constexpr const_iterator begin() const noexcept {
            return storage_.data();
        }

        [[nodiscard]] constexpr iterator end() noexcept
        requires (!std::is_const_v<value_type>) {
            return begin() + storage_.size();
        }

        [[nodiscard]] constexpr const_iterator end() const noexcept {
            return begin() + storage_.size();
        }

        [[nodiscard]] constexpr reverse_iterator rbegin() noexcept
        requires (!std::is_const_v<value_type>) {
            return std::reverse_iterator{ end() };
        }

        [[nodiscard]] constexpr reverse_iterator rend() noexcept
        requires (!std::is_const_v<value_type>) {
            return std::reverse_iterator{ begin() };
        }

        [[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept {
            return std::reverse_iterator{ end() };
        }

        [[nodiscard]] constexpr const_reverse_iterator rend() const noexcept {
            return std::reverse_iterator{ begin() };
        }

        [[nodiscard]] constexpr const_iterator cbegin() const noexcept {
            return storage_.data();
        }

        [[nodiscard]] constexpr const_iterator cend() const noexcept {
            return begin() + storage_.size();
        }

        [[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept {
            return std::reverse_iterator{ cend() };
        }

        [[nodiscard]] constexpr const_reverse_iterator crend() const noexcept {
            return std::reverse_iterator{ cbegin() };
        }

        /** @brief Returns an iterator pointing to the n-th element in the vector.
        *   @param n Index of the element to access.
        *   @warning: No bound checks are performed
        */
        [[nodiscard]] constexpr iterator nth(size_type n) noexcept {
            return begin() + n;
        }

        /** @brief Returns a const_iterator pointing to the n-th element in the vector.
        *   @param n Index of the element to access.
        *   @warning: No bound checks are performed
        */
        [[nodiscard]] constexpr const_iterator nth(size_type n) const noexcept {
            return cbegin() + n;
        }

        /**
         * @brief Returns the index of the element {it} points to.
         */
        [[nodiscard]] constexpr size_type index_of(const_iterator it) const noexcept {
            return static_cast<size_type>(it - this->begin());
        }

        /****************************** MODIFIERS ******************************/

        //////////////////////////////////////////////
        //            *_emplace_back                //
        //////////////////////////////////////////////

        /**
         * @brief Construct an element in-place at the end of the vector.
         * @return Reference to the element constructed.
         * @warning UB if size() == max_size().
         */
        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr reference unchecked_emplace_back(Args&&... args)
        noexcept(std::is_nothrow_constructible_v<value_type, Args&&...>) {
            std::construct_at(this->end(), std::forward<Args>(args)...);
            storage_.unchecked_set_size(this->size() + 1U);
            return this->back();
        }

        /**
         * @brief Construct an element in-place at the end of the vector.
         * @return Pointer to the element constructed. nullptr if size() == max_size().
         */
        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr pointer try_emplace_back(Args&&... args)
        noexcept(std::is_nothrow_constructible_v<value_type, Args&&...>) {
            if (this->size() == max_size()) [[unlikely]] {
                return nullptr;
            }
            return &this->unchecked_emplace_back(std::forward<Args>(args)...);
        }

        /**
         * @brief Construct an element in-place at the end of the vector.
         * @return Reference to the element constructed.
         * @throws std::bad_alloc if size() == max_size().
         */
        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr reference emplace_back(Args&&... args) {
            if (this->size() == max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            return this->unchecked_emplace_back(std::forward<Args>(args)...);
        }

        //////////////////////////////////////////////
        //              *_push_back                //
        //////////////////////////////////////////////

        constexpr reference unchecked_push_back(const_reference val)
        noexcept(std::is_nothrow_copy_constructible_v<value_type>) {
            std::construct_at(this->end(), val);
            storage_.unchecked_set_size(this->size() + 1U);
            return this->back();
        }

        constexpr reference unchecked_push_back(value_type&& val)
        noexcept(std::is_nothrow_move_constructible_v<value_type>) {
            std::construct_at(this->end(), std::move(val));
            storage_.unchecked_set_size(this->size() + 1U);
            return this->back();
        }

        constexpr pointer try_push_back(const_reference val)
        noexcept(std::is_nothrow_copy_constructible_v<value_type>) {
            if (this->size() == max_size()) [[unlikely]] {
                return nullptr;
            }
            return &this->unchecked_push_back(val);
        }

        constexpr pointer try_push_back(value_type&& val)
        noexcept(std::is_nothrow_move_constructible_v<value_type>) {
            if (this->size() == max_size()) [[unlikely]] {
                return nullptr;
            }
            return &this->unchecked_push_back(std::move(val));
        }

        constexpr reference push_back(const_reference val) {
            if (this->size() == max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            return this->unchecked_push_back(val);
        }

        constexpr reference push_back(value_type&& val) {
            if (this->size() == max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            return this->unchecked_push_back(std::move(val));
        }

        //////////////////////////////////////////////
        //               *_pop_back                 //
        //////////////////////////////////////////////

        /**
         * @brief: Removes the last element of the vector.
         * @note: UB if the vector is empty.
         */
        constexpr void unchecked_pop_back()
        noexcept(std::is_nothrow_destructible_v<value_type>) {
            if constexpr (!std::is_trivially_destructible_v<value_type>) {
                std::destroy_at(this->end() - 1);
            }
            storage_.unchecked_set_size(this->size() - 1U);
        }

        /**
         * @brief: Removes the last element of the vector with empty check.
         * @note: No-operation for an empty vector.
         */
        constexpr void pop_back()
        noexcept(std::is_nothrow_destructible_v<value_type>) {
            if (!this->empty()) [[likely]] {
                this->unchecked_pop_back();
            }
        }

        //////////////////////////////////////////////
        //             *_append_range               //
        //////////////////////////////////////////////

        /**
         * @brief: Append elements in [first, last) to the end of the vector.
         *
         * @tparam Iter Input iterator or above. value_type must be constructible from iter_reference_t<Iter>.
         * @tparam Sentinel Sentinel for Iter.
         *
         * @throws _ Any exceptions thrown during the construction of elements. Strong exception safety.
         * @warning: UB if distance(first, last) + current size > N.
         */
        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        constexpr void unchecked_append_range(Iter first, Sentinel last)
        noexcept(std::is_nothrow_constructible_v<value_type, std::iter_reference_t<Iter>>) {
            if constexpr (std::forward_iterator<Iter>) {
                const auto size = std::ranges::distance(first, last);
                this->storage_.append_with_size(first, size);
            } else {
                this->storage_.append_with_sentinel(std::move(first), last);
            }
        }

        /**
         * @brief Append elements from the given range to the end of the vector.
         * @tparam Rng Input range or above. value_type must be constructible from range_reference_t<Rng>
         *
         * @throws _ Any exceptions thrown during the construction of elements. Strong exception safety.
         * @warning: UB if size(rng) + current size > N.
         * @note: If Rng&& is a rvalue, elements in the range will be moved from it.
         */
        template <concepts::compatible_range<value_type> Rng>
        constexpr void unchecked_append_range(Rng&& rng)
        noexcept(std::is_nothrow_constructible_v<value_type, std::ranges::range_reference_t<Rng&&>>) {
            unchecked_append_range(detail::range_begin(std::forward<Rng>(rng)),
                                   detail::range_end(std::forward<Rng>(rng)));
        }

        /**
         * @brief Append elements from [first, last) to the end of the vector until capacity is reached.
         * @tparam Iter Input iterator or above. value_type must be constructible from iter_reference_t<Iter>.
         * @tparam Sentinel Sentinel for Iter.
         * @return An iterator pointing to the first element not appended to the vector, or a sentinel
         *         if no such element exists.
         *
         * @throw  _ Any exceptions thrown during the construction of elements. Strong exception safety.
         */
        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        constexpr decltype(auto) try_append_range(Iter first, Sentinel last)
        noexcept(std::is_nothrow_constructible_v<value_type, std::iter_reference_t<Iter>>) {
            if constexpr (std::forward_iterator<Iter>) {
                const auto dist = static_cast<size_type>(std::ranges::distance(first, last));
                const auto k = std::min(static_cast<size_type>(N - this->size()), dist);
                this->storage_.append_with_size(first, k);
                return std::next(first, k);
            } else {
                auto [until, cnt] =
                    this->storage_.append_with_sentinel(std::move(first), last);
                return until;
            }
        }

        /**
         * @brief Append elements from the given range to the end of the vector until capacity is reached.
         * @tparam Rng Input range or above. value_type must be constructible from range_reference_t<Rng>.
         *
         * @return An iterator pointing to the first element not appended to the vector, or a sentinel
         *         if no such element exists.
         *
         * @throw  _ Any exceptions thrown during the construction of elements. Strong exception safety.
         * @note: If Rng&& is a rvalue, elements in the range will be moved from it.
         */
        template <concepts::compatible_range<value_type> Rng>
        constexpr decltype(auto) try_append_range(Rng&& rng)
        noexcept(std::is_nothrow_constructible_v<value_type, std::ranges::range_reference_t<Rng&&>>) {
            return try_append_range(detail::range_begin(std::forward<Rng>(rng)),
                                    detail::range_end(std::forward<Rng>(rng)));
        }

        /**
         * @brief: Append elements in [first, last) to the end of the vector.
         * @tparam Iter Input iterator or above. value_type must be constructible from iter_reference_t<Iter>
         * @tparam Sentinel Sentinel for Iter
         *
         * @throws std::bad_alloc if distance(first, last) + current size > N. Strong exception
         *         guarantee.
         * @throws _ Any exceptions thrown during the construction of elements. Strong exception
         *         guarantee.
         */
        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        constexpr void append_range(Iter first, Sentinel last) {
            if constexpr (std::forward_iterator<Iter>) {
                const auto cnt = std::ranges::distance(first, last);
                if (cnt + this->size() > max_size()) [[unlikely]] {
                    throw std::bad_alloc{};
                }
                this->storage_.append_with_size(first, cnt);
            } else {
                auto old_size = this->size();
                auto [until, cnt] =
                    this->storage_.append_with_sentinel(std::move(first), last);
                if (until != last) [[unlikely]] {
                    std::destroy_n(this->data() + old_size, cnt);
                    storage_.unchecked_set_size(old_size);
                    throw std::bad_alloc{};
                }
            }
        }

        /**
         * @brief: Append elements from the given range to the end of the vector.
         * @tparam Rng: input_range or above. value_type must be constructible from range_reference_t<Rng>
         *
         * @throws std::bad_alloc if size(rng) + current size > N. Strong exception
         *         guarantee.
         * @throws _ Any exceptions thrown during the construction of elements. Strong exception
         *         guarantee.
         * @note: If Rng&& is a rvalue, elements in the range will be moved from it.
         */
        template <concepts::compatible_range<value_type> Rng>
        constexpr void append_range(Rng&& rng) {
            append_range(detail::range_begin(std::forward<Rng>(rng)),
                         detail::range_end(std::forward<Rng>(rng)));
        }


        //////////////////////////////////////////////
        //                 *_emplace                //
        //////////////////////////////////////////////

        /**
         * @brief Construct an element in-place before cpos.
         * @tparam Args Variadic template parameters pack from which value_type is constructible.
         *
         * @return An iterator pointing to the element constructed.
         * @warning: UB if the vector is already full or cpos is out-of-range.
         *
         * @throw _ Any exceptions thrown in the construction of the new element. Strong exception safety.
         * @throw _ Any exceptions thrown in the move assignment of elements. Basic exception safety only.
         */
        template <typename ...Args>
        constexpr iterator unchecked_emplace(const_iterator cpos, Args&& ...args)
        noexcept(std::is_nothrow_move_assignable_v<value_type> &&
                 std::is_nothrow_constructible_v<value_type, Args...>)
        requires std::constructible_from<value_type, Args&&...> &&
                 std::is_move_assignable_v<value_type> {
            auto pos = this->begin() + (cpos - this->begin());
            const auto dist = this->end() - pos;
            this->unchecked_emplace_back(std::forward<Args>(args)...);
            std::rotate(pos, pos + dist, this->end());
            return pos;
        }

        /**
         * @brief Construct an element in-place before cpos.
         * @tparam Args Variadic template parameters pack from which value_type must be constructible.
         *
         * @return An iterator pointing to the element constructed.
         * @throw std::bad_alloc if the vector is already full.
         * @throw std::out_of_range if cpos is out-of-range (not in [begin(), end()])
         * @throw _ Any exceptions thrown in the construction of the new element. Strong exception safety.
         * @throw _ Any exceptions thrown in the move assignment of elements. Basic exception safety only.
         */
        template <typename ...Args>
        constexpr iterator emplace(const_iterator cpos, Args&& ...args)
        requires std::constructible_from<value_type, Args&&...> &&
                 std::is_move_assignable_v<value_type> {
            if (this->is_full()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            if (cpos < this->begin() || cpos - this->begin() > size()) [[unlikely]] {
                throw std::out_of_range{"inplace_vector::emplace()"};
            }
            return this->unchecked_emplace(cpos, std::forward<Args>(args)...);
        }

        //////////////////////////////////////////////
        //                 insert_*                 //
        //////////////////////////////////////////////

        /**
         * @brief: Insert {cnt} copies of {value} before {cpos}.
         * @return: An iterator pointing to the first element inserted.
         * @throws std::bad_alloc if distance(first, last) + current size > N. Strong exception safety.
         * @throws std::out_of_range if cpos is not in [begin(), end()]. Strong exception safety.
         * @throws _ Any exceptions thrown during the copy construction of new elements. Strong exception safety.
         * @throws _ Any exceptions thrown during the move construction/assignment of elements in gap creation.
         *         Basic exception guarantee only.
         */
        constexpr iterator insert(const_iterator cpos, const size_type cnt, const_reference value)
        requires std::copy_constructible<value_type> && std::is_move_assignable_v<value_type> {
            const auto old_size = this->size();
            if (old_size + cnt > max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            if (cpos < this->begin() || cpos - this->begin() > size()) [[unlikely]] {
                throw std::out_of_range{"inplace_vector::insert()"};
            }
            auto pos = this->begin() + (cpos - this->begin());
            const auto tail_len = this->end() - pos;

            if constexpr (std::is_nothrow_constructible_v<value_type, const_reference> &&
                          std::is_nothrow_move_assignable_v<value_type> &&
                          std::is_nothrow_move_constructible_v<value_type>) {
                if (cnt >= tail_len) {
                    std::uninitialized_move_n(pos, tail_len, pos + cnt);  // make space
                    std::fill_n(pos, tail_len, value);
                    std::uninitialized_fill_n(pos + tail_len, cnt - tail_len, value);
                } else {
                    const auto tail = this->end() - cnt;
                    std::uninitialized_move_n(tail, cnt, this->end());
                    std::move_backward(pos, tail, this->end());
                    std::fill_n(pos, cnt, value);
                }
                this->storage_.unchecked_set_size(old_size + cnt);
            } else {
                this->storage_.fill_with_size(cnt, value);
                std::rotate(pos, pos + tail_len, this->end());
            }
            return pos;
        }

        /**
         * @brief Construct an element from {value} before cpos.
         *
         * @return An iterator pointing to the element inserted.
         * @throw std::bad_alloc if the vector is already full.
         * @throw std::out_of_range if cpos is out-of-range (not in [begin(), end()])
         * @throw _ Any exceptions thrown in the construction of the new element. Strong exception safety.
         * @throw _ Any exceptions thrown in the move assignment of elements. Basic exception safety only.
         */
        constexpr iterator insert(const_iterator cpos, const_reference value)
        requires std::copy_constructible<value_type> && std::is_move_assignable_v<value_type> {
            if (this->size() == max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            if (cpos < this->begin() || cpos - this->begin() > size()) [[unlikely]] {
                throw std::out_of_range{"inplace_vector::insert()"};
            }
            return this->unchecked_emplace(cpos, value);
        }

        /**
         * @brief Construct an element from {value} before cpos.
         *
         * @return An iterator pointing to the element inserted.
         * @throw std::bad_alloc if the vector is already full.
         * @throw std::out_of_range if cpos is out-of-range (not in [begin(), end()])
         * @throw _ Any exceptions thrown in the construction of the new element. Strong exception safety.
         * @throw _ Any exceptions thrown in the move assignment of elements. Basic exception safety only.
         */
        constexpr iterator insert(const_iterator cpos, value_type&& value)
        requires std::move_constructible<value_type> && std::is_move_assignable_v<value_type> {
            if (this->size() == max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            }
            if (cpos < this->begin() || cpos - this->begin() > size()) [[unlikely]] {
                throw std::out_of_range{"inplace_vector::insert()"};
            }
            return this->unchecked_emplace(cpos, std::move(value));
        }

        /**
         * @brief: Inserts elements from [first, last) before {cpos}.
         * @tparam Iter Input iterator or above. value_type must be constructible from iter_reference_t<Iter>.
         * @tparam Sentinel Sentinel for Iter.
         * @return An iterator pointing to the first element inserted.
         *
         * @throws std::bad_alloc if distance(first, last) + current size > N. Strong exception safety.
         * @throws std::out_of_range if cpos is not in [begin(), end()]. Strong exception safety.
         * @throws _ Any exceptions thrown during the copy construction of elements from the given range.
         *         Strong exception safety.
         * @throws _ Any exceptions thrown during the move construction/assignment of elements in gap creation.
         *         Basic exception guarantee only.
         */
        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        requires std::is_move_assignable_v<value_type>
        constexpr iterator insert(const_iterator cpos, Iter first, Sentinel last) {
            if (cpos < this->begin() || cpos - this->begin() > size()) [[unlikely]] {
                throw std::out_of_range{"inplace_vector::insert()"};
            }
            const auto pos = this->begin() + (cpos - this->begin());
            const auto dist = this->end() - pos;

            if constexpr (std::forward_iterator<Iter> &&
                          std::is_nothrow_constructible_v<value_type, std::iter_reference_t<Iter>> &&
                          std::is_nothrow_move_assignable_v<value_type> &&
                          std::is_nothrow_move_constructible_v<value_type>) {
                const auto old_size = this->size();
                const auto cnt = static_cast<size_type>(std::ranges::distance(first, last));
                if (old_size + cnt > max_size()) [[unlikely]] {
                    throw std::bad_alloc{};
                }
                if (cnt >= dist) {
                    std::uninitialized_move_n(pos, dist, pos + cnt);  // make space
                    std::ranges::copy_n(first, dist, pos);
                    auto mid_pt = std::ranges::next(first, dist);
                    std::uninitialized_copy_n(mid_pt, cnt - dist, pos + dist);
                } else {
                    std::uninitialized_move_n(this->end() - cnt, cnt, this->end());
                    std::move_backward(pos, this->end() - cnt, this->end());
                    std::copy_n(first, cnt, pos);
                }
                this->storage_.unchecked_set_size(old_size + cnt);
            } else {
                this->append_range(std::move(first), last);
                std::rotate(pos, pos + dist, this->end()); // If this throws, basic guarantee only.
            }
            return pos;
        }

        /**
         * @brief: Inserts elements from the given initializer list before {cpos}
         * @tparam VTy: Any type from which value_type is constructible.
         * @return An iterator pointing to the first element inserted.
         *
         * @throws std::bad_alloc if distance(first, last) + current size > N. Strong exception safety.
         * @throws std::out_of_range if cpos is not in [begin(), end()]. Strong exception safety.
         * @throws _ Any exceptions thrown during the copy construction of elements from the given range.
         *         Strong exception safety.
         * @throws _ Any exceptions thrown during the move construction/assignment of elements in gap creation.
         *         Basic exception guarantee only.
         */
        template <concepts::can_construct<value_type> VTy>
        requires std::is_move_assignable_v<value_type>
        constexpr iterator insert(const_iterator cpos, std::initializer_list<VTy> il) {
            return this->insert(cpos, il.begin(), il.end());
        }

        /**
         * @brief: Inserts elements from the given range before {cpos}
         * @tparam Rng: Input range or above. value_type must be constructible from range_reference_t<Rng>
         * @return An iterator pointing to the first element inserted.
         *
         * @throws std::bad_alloc if distance(first, last) + current size > N. Strong exception safety.
         * @throws std::out_of_range if cpos is not in [begin(), end()]. Strong exception safety.
         * @throws _ Any exceptions thrown during the copy construction of elements from the given range.
         *         Strong exception safety.
         * @throws _ Any exceptions thrown during the move construction/assignment of elements in gap creation.
         *         Basic exception guarantee only.
         * @note: If the given range is a rvalue, its elements are moved from it.
         */
        template <concepts::compatible_range<value_type> Rng>
        requires std::is_move_assignable_v<value_type>
        constexpr iterator insert_range(const_iterator cpos, Rng&& rng) {
            return this->insert(cpos, detail::range_begin(std::forward<Rng>(rng)),
                                      detail::range_end(std::forward<Rng>(rng)));
        }

        //////////////////////////////////////////////
        //                 resize                   //
        //////////////////////////////////////////////

        /**
         * @brief Resizes the vector to [new_size]. If current size > new_size, extra elements
         *        are destroyed. If current size < new_size, empty slots are filled with [value].
         *
         * @param new_size Size to which the vector is resized
         * @param value Value to fill the vector if new_size > current size
         *
         * @throw _ Any exceptions throw during the copy construction of value_type.
         *        Strong exception safety.
         */
        constexpr void resize(size_type new_size, const_reference value)
        requires std::copy_constructible<value_type> {
            const auto curr_size = this->size();
            if (new_size == curr_size) [[unlikely]] {
                return;
            } if (new_size > max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            } if (new_size > curr_size) {
                storage_.fill_with_size(new_size - curr_size, value);
            } else {  // new_size < this->size()
                if constexpr (!std::is_trivially_destructible_v<value_type>) {
                    std::destroy_n(this->begin() + new_size, curr_size - new_size);
                }
                storage_.unchecked_set_size(new_size);
            }
        }

        /**
         * @brief Resizes the vector to new_size. If current size > new_size, extra elements
         *        are destroyed. If current size < new_size, empty slots are default initialized.
         *
         * @param new_size Size to which the vector is resized
         *
         * @throw _ Any exceptions throw during the default construction of value_type.
         *        Strong exception safety.
         */
        constexpr void resize(size_type new_size)
        requires std::default_initializable<value_type> {
            const auto curr_size = this->size();
            if (new_size == curr_size) [[unlikely]] {
                return;
            } if (new_size > max_size()) [[unlikely]] {
                throw std::bad_alloc{};
            } if (new_size > curr_size) {
                std::uninitialized_default_construct_n(this->begin() + curr_size, new_size - curr_size);
                storage_.unchecked_set_size(new_size);
            } else {  // new_size < this->size()
                if constexpr (!std::is_trivially_destructible_v<value_type>) {
                    std::destroy_n(this->begin() + new_size, curr_size - new_size);
                }
                storage_.unchecked_set_size(new_size);
            }
        }

        //////////////////////////////////////////////
        //                  erase                   //
        //////////////////////////////////////////////

        /**
         * @brief Erases the elements in [first, last).
         * @return An iterator pointing to the exact position {first} points to.
         * @throw _ Any exceptions thrown during the move assignment of the elements.
         *        Basic exception guarantee only.
         */
        constexpr iterator erase(const_iterator first, const_iterator last)
        noexcept(std::is_nothrow_move_assignable_v<value_type>)
        requires std::is_move_assignable_v<value_type> {
            const auto diff = last - first;
            iterator pos { this->begin() + (first - this->begin()) };
            iterator epos { pos + diff };
            std::move(epos, this->end(), pos);  // move elements to the front
            std::destroy(this->end() - diff, this->end());  // destroy trailing elements
            const auto new_size = this->size() - static_cast<size_type>(diff);
            this->storage_.unchecked_set_size(new_size);
            return pos;
        }

        /**
         * @brief Erases the element pointed to by {cpos}.
         * @return An iterator pointing to the exact position {cpos} points to.
         * @throw _ Any exceptions thrown during the move assignment of the elements.
         *        Basic exception guarantee only.
         */
        constexpr iterator erase(const_iterator cpos)
        noexcept(std::is_nothrow_move_assignable_v<value_type>)
        requires std::is_move_assignable_v<value_type> {
            return this->erase(cpos, cpos + 1);
        }

        /**
         * @brief Erases all elements equal to {value}
         * @return The new end position.
         * @throw _ Any exceptions thrown during the move assignment of the elements.
         *        Basic exception guarantee only.
         */
        constexpr iterator erase(const_reference value)
        noexcept(std::is_nothrow_move_assignable_v<value_type>)
        requires std::is_move_assignable_v<value_type> && std::equality_comparable<value_type> {
            return this->erase(std::remove(begin(), end(), value), end());
        }

        /**
         * @brief Erases all elements for which {pred} returns true.
         * @return The new end position.
         * @throw _ Any exceptions thrown by the predicate, or during the move assignment of the elements.
         *        Basic exception guarantee only.
         */
        template <std::predicate<value_type> Pred>
        constexpr iterator erase_if(Pred&& pred)
        noexcept(std::is_nothrow_invocable_v<Pred, value_type> &&
                 std::is_nothrow_move_assignable_v<value_type>) {
            return this->erase(
                std::remove_if(this->begin(), this->end(), std::forward<Pred>(pred)),
                this->end());
        }

        constexpr void clear() noexcept(noexcept(storage_.clear())) {
            storage_.clear();
        }

        /****************************** UTILITIES ******************************/

        [[nodiscard]] constexpr bool contains(const_reference val) const noexcept {
            return std::find(cbegin(), cend(), val) != cend();
        }

        constexpr void swap(inplace_vector& other)
        noexcept(std::is_nothrow_move_constructible_v<storage_type>
              && std::is_nothrow_move_assignable_v<storage_type>)
        requires std::movable<value_type> {
            auto temp = std::move(other);
            other = std::move(*this);
            *this = std::move(temp);
        }

        constexpr friend void swap(inplace_vector& lhs, inplace_vector& rhs)
        noexcept(noexcept(lhs.swap(rhs)))
        requires requires() { lhs.swap(rhs); } {
            lhs.swap(rhs);
        }

        constexpr friend bool operator==(const inplace_vector& x, const inplace_vector& y)
        noexcept(concepts::nothrow_equality_comparable<value_type>)
        requires std::equality_comparable<value_type> {
            return x.size() == y.size() && std::ranges::equal(x, y);
        }

        /**
         * @brief Compares the two given vector lexicographically. Requires value_type to be at least LessComparable.
         * @return The comparison result. If value_type is ThreeWayComparable, return type is
         *         compare_three_way_result_t<value_type>. Otherwise, return type is std::weak_ordering.
         */
        constexpr friend auto operator<=>(const inplace_vector& lhs, const inplace_vector& rhs)
        noexcept((std::three_way_comparable<value_type>
                && concepts::nothrow_three_way_comparable<value_type>)
            ||  (!std::three_way_comparable<value_type>
                && concepts::nothrow_less_comparable<value_type>))
        requires concepts::less_comparable<value_type> {
            using common_ordering = std::conditional_t<std::three_way_comparable<value_type>,
                                                       std::compare_three_way_result_t<value_type>,
                                                       std::weak_ordering>;
            if constexpr (std::three_way_comparable<value_type>) {
                return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(),
                                                              rhs.begin(), rhs.end());
            } else {
                const auto sz = std::min(lhs.size(), rhs.size());
                for (std::size_t i = 0; i < sz; ++i) {
                    if (lhs[i] < rhs[i]) {
                        return common_ordering::less;
                    } if (rhs[i] < lhs[i]) {
                        return common_ordering::greater;
                    }
                }
                return static_cast<common_ordering>(lhs.size() <=> rhs.size());
            }
        }

    }; // class inplace_vector


    /************************* CTAD guides *************************/

    template <typename T, std::size_t N>
    inplace_vector(const inplace_vector<T, N>&) -> inplace_vector<T, N>;

    template <typename T, std::size_t N>
    inplace_vector(inplace_vector<T, N>&&) -> inplace_vector<T, N>;

    template <typename T, typename ...Args>
    requires (std::same_as<T, Args> && ...)
    inplace_vector(T, Args...) -> inplace_vector<T, 1 + sizeof...(Args)>;

}


#endif //URLICHT_INPLACE_VECTOR_H
