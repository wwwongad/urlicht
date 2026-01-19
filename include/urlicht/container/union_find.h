#ifndef URLICHT_UNION_FIND_H
#define URLICHT_UNION_FIND_H
#include <concepts>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <vector>
#include <urlicht/concepts_utility.h>
#include <urlicht/container/inplace_vector.h>

/******************************* SYNOPSIS ********************************
    Base class (protected helpers)
    - const_reference get_ensured_root(const_reference) const
    - const_reference get_ensured_root_and_compress(const_reference)
    - void unite_existing_roots(const_reference root_x, const_reference root_y)

    Base class (public)
    - bool contains(const_reference) const noexcept
    - size_type set_size(const_reference) const noexcept    // only when TrackSize = true
    - bool is_root(const_reference) const noexcept
    - void reset_all()

    Map-backed specific (public)
    - template<Key>                 bool try_emplace(Key&&)
    - template <InputIt, Sentinel>  void insert(InputIt first, Sentinel last)
    - template <U>                  void insert(std::initializer_list<U>)
    - template <Rng>                void insert_range(Rng&&)

    Array-backed specific (public)
    - void resize(size_type)

    Derived class (public)
    - const_reference find_or_insert(const_reference)
    - const_reference unchecked_find(const_reference)
    - const_reference unchecked_find(const_reference) const
    - void unite_new(const_reference, const_reference)
    - bool try_unite(const_reference, const_reference)
    - void unchecked_unite(const_reference, const_reference)
    - size_type set_count() const noexcept
    - bool same_set(const_reference, const_reference) const noexcept

    Iteration (Public, const-only)
    - const_iterator begin() const noexcept
    - const_iterator end() const noexcept
    - const_reverse_iterator rbegin() const noexcept
    - const_reverse_iterator rend() const noexcept
    - const_iterator cbegin() const noexcept
    - const_iterator cend() const noexcept
    - const_reverse_iterator crbegin() const noexcept
    - const_reverse_iterator crend() const noexcept

    Capacity (Public)
    - bool empty() const noexcept
    - size_type size() const noexcept
    - std::size_t ssize() const noexcept
    - size_type max_size() const noexcept
    - size_type capacity() const noexcept                     // when available
    - void reserve(size_type)                                 // when reservable

    Utilities (Public)
    - void clear() noexcept                                   // when supported by KeyEqual
    - void swap(basic_union_find&) noexcept                   // when swappable
**************************************************************************/

namespace urlicht {
    namespace detail {
        /********************** UF CONCEPTS CHECKS AND DEFAULT TYPES *********************/

        // Parent storage can be either unordered_map or random_access_container.
        template <typename Cont, typename T>
        concept valid_parent_storage =
            (concepts::unordered_map<Cont> &&
                std::same_as<typename Cont::key_type, typename Cont::mapped_type> &&
                std::same_as<typename Cont::key_type, T>)
        // random_access_container -> value type must be unsigned integral
        ||  (concepts::random_access_container<Cont> &&
                std::integral<T> &&
                std::same_as<typename Cont::value_type, T>);

        template <typename T>
        using default_parent_storage_t =
            std::conditional_t<std::integral<T>, std::vector<T>, std::unordered_map<T, T>>;

        // If TrackSize is false, then size storage can be whatever type.
        // Otherwise, size storage must match parent storage
        template <typename SizeStorage, typename T, bool TrackSize, typename ParentStorage>
        concept valid_size_storage =
            !TrackSize || (
                (concepts::unordered_map<ParentStorage> && concepts::unordered_map<SizeStorage> &&
                 std::same_as<typename SizeStorage::key_type, T> &&
                 std::unsigned_integral<typename SizeStorage::mapped_type>)
            ||  (concepts::random_access_container<ParentStorage> &&
                 concepts::random_access_container<SizeStorage> &&
                 std::unsigned_integral<typename SizeStorage::value_type>)
            );

        // Default size storage binds mapped/value types to ParentStorage::size_type.
        // For maps: std::unordered_map<T, size_type>; for arrays: std::vector<size_type>.
        template <typename T, bool TrackSize, typename ParentStorage>
        using default_size_storage_t =
            std::conditional_t<TrackSize,
                std::conditional_t<concepts::unordered_map<ParentStorage>,
                    std::unordered_map<T, typename ParentStorage::size_type>,
                    std::vector<typename ParentStorage::size_type>>,
                std::monostate>;

        // Optimizes size storage away when TrackSize is false
        template <typename T, bool TrackSize, typename SizeStorage>
        using adaptive_size_storage_t = std::conditional_t<TrackSize, SizeStorage, std::monostate>;

        // Uses the key_equal type in ParentStorage for unordered_map. Otherwise, defaults to std::equal_to<>
        template <typename ParentStorage, bool IsMap>
        struct default_key_equal_impl {
            using type = std::equal_to<>;
        };

        template <typename ParentStorage>
        struct default_key_equal_impl<ParentStorage, true> {
            using type = typename ParentStorage::key_equal;
        };

        template <typename ParentStorage>
        using default_key_equal_t =
            typename default_key_equal_impl<ParentStorage, concepts::unordered_map<ParentStorage>>::type;


        /************************** IMPL FOR MAP-BASED UF *************************/

        template <
            typename T,
            bool TrackSize,
            typename ParentStorage,
            typename SizeStorage,
            typename KeyEqual>
        class map_uf_base_ {
        public:
            // Necessary ones only. Others are defined in the derived union-find class
            using parent_storage = ParentStorage;
            using size_storage = adaptive_size_storage_t<T, TrackSize, SizeStorage>;
            using value_type = T;
            using size_type = typename parent_storage::size_type;
            using key_equal = KeyEqual;
        protected:
            parent_storage parents_{};
            [[no_unique_address]] size_storage sizes_{}; // May be std::monostate
            size_type set_count_{0U};
            [[no_unique_address]] key_equal key_equal_{};

            [[nodiscard]]   // UB if val does not exist
            constexpr const value_type& get_ensured_root(const value_type& val) const {
                const value_type* prev = &val;
                typename parent_storage::const_iterator next;
                // Zero extra copies
                while (true) {
                    next = this->parents_.find(*prev);
                    if (this->key_equal_(next->second, *prev)) {
                        return next->second;
                    }
                    prev = &next->second;
                }
            }

            [[nodiscard]]
            constexpr const value_type& get_ensured_root_and_compress(const value_type& val) {
                const auto& root = get_ensured_root(val);
                auto next = this->parents_.find(val);
                while (!this->key_equal_(next->second, root)) {
                    auto curr = next;
                    next = this->parents_.find(next->second);
                    curr->second = root;    // Copies only occur here
                }
                return root;
            }

            constexpr void unite_existing_roots(const value_type& root_x, const value_type& root_y) {
                if (this->key_equal_(root_x, root_y)) [[unlikely]] {
                    return;
                }
                if constexpr (TrackSize) {
                    auto& x_size = this->sizes_.find(root_x)->second;
                    auto& y_size = this->sizes_.find(root_y)->second;
                    if (x_size > y_size) {
                        // If the set x belongs to is larger, merge the set of y to it
                        this->parents_.find(root_y)->second = root_x;
                        x_size += y_size;
                    } else {
                        // Otherwise, merge the set of x to the set of y
                        this->parents_.find(root_x)->second = root_y;
                        y_size += x_size;
                    }
                } else {
                    this->parents_.find(root_x)->second = root_y;
                }
                --this->set_count_;
            }

        public:
            /**************** Map-based union-find specific methods *****************/

            constexpr map_uf_base_() noexcept
            requires std::default_initializable<key_equal> = default;

            template <concepts::can_construct<key_equal> KeyEq>
            explicit constexpr map_uf_base_(KeyEq&& key_eq)
            noexcept(std::is_nothrow_constructible_v<key_equal, KeyEq&&>)
            : key_equal_{std::forward<KeyEq>(key_eq)} {  }

            template <concepts::compatible_iterator<value_type> Iter,
                      std::sentinel_for<Iter> Sentinel>
            constexpr map_uf_base_(Iter first, Sentinel last)
            requires std::default_initializable<key_equal>
            : map_uf_base_(first, last, key_equal{}) {  }

            template <concepts::compatible_iterator<value_type> Iter,
                      std::sentinel_for<Iter> Sentinel,
                      concepts::can_construct<key_equal> KeyEq>
            constexpr map_uf_base_(Iter first, Sentinel last, KeyEq&& key_eq)
            : key_equal_{ std::forward<KeyEq>(key_eq) } {
                this->insert(first, last);
            }

            template <concepts::compatible_range<value_type> Rng>
            explicit constexpr map_uf_base_(Rng&& rng)
            requires std::default_initializable<key_equal>
            : map_uf_base_(std::forward<Rng>(rng), key_equal{}) {  }

            template <concepts::compatible_range<value_type> Rng,
                      concepts::can_construct<key_equal> KeyEq>
            constexpr map_uf_base_(Rng&& rng, KeyEq&& key_eq)
            : key_equal_{ std::forward<KeyEq>(key_eq) } {
                this->insert_range(std::forward<Rng>(rng));
            }

            constexpr map_uf_base_(const map_uf_base_&) = default;

            constexpr map_uf_base_(map_uf_base_&& other)
            noexcept(std::is_nothrow_move_constructible_v<parent_storage> &&
                     std::is_nothrow_move_constructible_v<size_storage> &&
                     std::is_nothrow_move_constructible_v<key_equal>)
            : parents_{std::move(other.parents_)}, sizes_{std::move(other.sizes_)},
              set_count_{other.set_count_}, key_equal_{std::move(other.key_equal_)} {
                other.set_count_ = 0;
            }

            constexpr map_uf_base_& operator=(const map_uf_base_&) = default;

            constexpr map_uf_base_& operator=(map_uf_base_&& other)
            noexcept(std::is_nothrow_move_assignable_v<parent_storage> &&
                     std::is_nothrow_move_assignable_v<size_storage> &&
                     std::is_nothrow_move_assignable_v<key_equal>) {
                if (this != &other) {
                    parents_ = std::move(other.parents_);
                    sizes_ = std::move(other.sizes_);
                    set_count_ = other.set_count_;
                    key_equal_ = std::move(other.key_equal_);
                    other.set_count_ = 0;
                }
                return *this;
            }

            constexpr ~map_uf_base_() = default;


            /**
             * @brief Attempts to emplace a value into the union-find and sets the
             *  parent of the value to itself.
             *
             * @return False if the value already exists. True otherwise.
             */
            template <typename Key>
            requires (!TrackSize) && std::constructible_from<value_type, Key&&>
            constexpr bool try_emplace(Key&& key) {
                const auto [it, inserted] = this->parents_.try_emplace(key, key);
                if (!inserted) {
                    return false;
                }
                ++this->set_count_;
                return true;
            }

            /**
             * @brief Attempts to emplace a value into the union-find and sets the
             *  parent of the value to itself. Provides the strong exception guarantee.
             *
             * @return False if the value already exists. True otherwise.
             */
            template <typename Key>
            requires TrackSize && std::constructible_from<value_type, Key&&>
            constexpr bool try_emplace(Key&& key) {
                // Strong exception guarantee
                bool p_done = false;
                typename parent_storage::iterator p_it;
                try {
                    bool inserted = false;
                    std::tie(p_it, inserted) = this->parents_.try_emplace(key, key);
                    if (!inserted) {
                        return false; // The element already exists
                    }
                    p_done = true;
                    this->sizes_.try_emplace(std::forward<Key>(key), 1);
                } catch (...) {
                    if (p_done) this->parents_.erase(p_it);  // This is noexcept
                    throw;
                }
                ++this->set_count_;
                return true;
            }

            template <concepts::compatible_iterator<value_type> Iter,
                      std::sentinel_for<Iter> Sentinel>
            constexpr void insert(Iter first, Sentinel last) {
                if constexpr (std::forward_iterator<Iter>) {
                    const auto m = this->parents_.size();
                    const auto n = static_cast<size_type>(std::ranges::distance(first, last));
                    this->parents_.reserve(m + n);
                    if constexpr (TrackSize) {
                        this->sizes_.reserve(m + n);
                    }
                }
                for (; first != last; ++first) {
                    this->try_emplace(*first);
                }
            }

            template <typename VTy>
            requires std::constructible_from<value_type, VTy>
            constexpr void insert(std::initializer_list<VTy> li) {
                this->insert(li.begin(), li.end());
            }

            template <concepts::compatible_range<value_type> Rng>
            constexpr void insert_range(Rng&& rng) {
                if constexpr (std::ranges::sized_range<Rng> || std::ranges::forward_range<Rng>) {
                    const auto m = this->parents_.size();
                    const auto n = static_cast<size_type>(std::ranges::distance(rng));
                    this->parents_.reserve(m + n);
                    if constexpr (TrackSize) {
                        this->sizes_.reserve(m + n);
                    }
                }
                for (auto&& elem : rng) {
                    this->try_emplace(std::forward<decltype(elem)>(elem));
                }
            }

            [[nodiscard]] constexpr bool contains(const value_type& x) const noexcept {
                return this->parents_.contains(x);
            }

            [[nodiscard]] constexpr size_type set_size(const value_type& x) const noexcept
            requires TrackSize {
                if (!this->contains(x)) {
                    return 0U;
                }
                return this->sizes_.find(this->get_ensured_root(x))->second;
            }

            [[nodiscard]] constexpr bool is_root(const value_type& x) const
            noexcept(std::is_nothrow_invocable_v<key_equal, const value_type&, const value_type&>) {
                return this->contains(x) && this->key_equal_(x, this->parents_.find(x)->second);
            }

            constexpr void reset_all() {
                for (auto& it : this->parents_) {
                    it.second = it.first;
                }
                if constexpr (TrackSize) {
                    for (auto& it : this->sizes_) {
                        it.second = 1U;
                    }
                }
                this->set_count_ = this->parents_.size();
            }

        };  // class map_uf_base_

        /************************** IMPL FOR ARRAY-BASED UF *************************/

        template <
            typename T,
            bool TrackSize,
            typename ParentStorage,
            typename SizeStorage,
            typename KeyEqual = std::equal_to<>>
        class array_uf_base_ {
        public:
            using parent_storage = ParentStorage;
            using size_storage = adaptive_size_storage_t<T, TrackSize, SizeStorage>;
            using value_type = T;
            using size_type = typename parent_storage::size_type;
            using key_equal = KeyEqual;
        protected:
            parent_storage parents_;  // No default initialization
            [[no_unique_address]] size_storage sizes_;
            size_type set_count_{0};
            [[no_unique_address]] key_equal key_equal_{};

            constexpr const value_type& get_ensured_root(value_type x) const
            noexcept(std::is_nothrow_invocable_v<key_equal, const value_type&, const value_type&>) {
                while (!this->key_equal_(x, this->parents_[x])) {
                    x = this->parents_[x];
                }
                return this->parents_[x];
            }

            // Called only if the element x exists
            constexpr const value_type& get_ensured_root_and_compress(value_type x)
            noexcept(std::is_nothrow_invocable_v<key_equal, const value_type&, const value_type&>) {
                const value_type& root = get_ensured_root(x);
                // Path compression
                while (!this->key_equal_(x, this->parents_[x])) {
                    value_type tmp = this->parents_[x];
                    this->parents_[x] = root;
                    x = tmp;
                }
                return root;
            }

            constexpr void unite_existing_roots(value_type root_x, value_type root_y)
            noexcept(std::is_nothrow_invocable_v<key_equal, const value_type&, const value_type&>) {
                if (this->key_equal_(root_x, root_y)) [[unlikely]] {
                    return;
                }
                if constexpr (TrackSize) {
                    if (this->sizes_[root_x] > this->sizes_[root_y]) {
                        // If the set x belongs to is larger, merge the set of y to it
                        this->parents_[root_y] = root_x;
                        this->sizes_[root_x] += this->sizes_[root_y];
                    } else {
                        // Otherwise, merge the set of x to the set of y
                        this->parents_[root_x] = root_y;
                        this->sizes_[root_y] += this->sizes_[root_x];
                    }
                } else {
                    this->parents_[root_x] = root_y;
                }
                --this->set_count_;
            }

        public:

            constexpr array_uf_base_() requires std::default_initializable<key_equal> = default;

            template <concepts::can_construct<key_equal> KeyEq>
            explicit constexpr array_uf_base_(KeyEq&& key_eq)
            : key_equal_{std::forward<KeyEq>(key_eq)} {   }

            explicit constexpr array_uf_base_(const size_type n)
            requires std::default_initializable<key_equal>
            : array_uf_base_(n, KeyEqual{}) {   }

            template <concepts::can_construct<key_equal> KeyEq>
            constexpr array_uf_base_(const size_type n, KeyEq&& eq)
            : parents_(n), set_count_{n}, key_equal_{ std::forward<KeyEq>(eq) } {
                std::iota(parents_.begin(), parents_.end(), 0);
                if constexpr (TrackSize) {
                    sizes_.assign(n, 1);
                }
            }

            constexpr array_uf_base_(const array_uf_base_&) = default;

            constexpr array_uf_base_(array_uf_base_&& other)
            noexcept(std::is_nothrow_move_constructible_v<parent_storage> &&
                     std::is_nothrow_move_constructible_v<size_storage> &&
                     std::is_nothrow_move_constructible_v<key_equal>)
            : parents_{std::move(other.parents_)}, sizes_{std::move(other.sizes_)},
                set_count_{other.set_count_}, key_equal_{std::move(other.key_equal_)} {
                other.set_count_ = 0;
            }

            constexpr array_uf_base_& operator=(const array_uf_base_&) = default;

            constexpr array_uf_base_& operator=(array_uf_base_&& other)
            noexcept(std::is_nothrow_move_assignable_v<parent_storage> &&
                     std::is_nothrow_move_assignable_v<size_storage> &&
                     std::is_nothrow_move_assignable_v<key_equal>) {
                if (this != &other) {
                    this->parents_ = std::move(other.parents_);
                    this->sizes_ = std::move(other.sizes_);
                    this->set_count_ = other.set_count_;
                    this->key_equal_ = std::move(other.key_equal_);
                    other.set_count_ = 0;
                }
                return *this;
            }

            constexpr ~array_uf_base_() = default;

            /********************** CORE METHODS **********************/

            /**
             * @warning: If T is unsigned integral, do NOT pass any negative number.
             * Otherwise, it will be converted to a very large unsigned int, which triggers
             * a resize to a massive size.
             */
            constexpr void resize(const size_type n) requires (!TrackSize) {
                const auto old_size = this->parents_.size();
                if (n <= old_size) {
                    throw std::logic_error("Union find does not support reducing elements");
                }
                this->parents_.resize(n);
                // parents_[i] = i
                std::iota(this->parents_.begin() + old_size, this->parents_.end(), old_size);
                this->set_count_ += n - old_size;
            }

            /**
             * @warning: If T is unsigned integral, do NOT pass any negative number.
             * Otherwise, it will be converted to a very large unsigned int, which triggers
             * a resize to a massive size.
             */
            constexpr void resize(const size_type n) requires TrackSize {
                const auto old_size = this->parents_.size();

                if (n <= old_size) {
                    throw std::logic_error("Union find does not support reducing elements");
                }
                bool s_ok = false;
                try {
                    this->sizes_.resize(n); s_ok = true;
                    this->parents_.resize(n);
                } catch (...) {
                    if (s_ok) this->sizes_.resize(old_size);
                    throw;
                }
                // parents_[i] = i
                std::iota(this->parents_.begin() + old_size, this->parents_.end(), old_size);
                // sizes_[i] = 1
                std::fill(this->sizes_.begin() + old_size, this->sizes_.end(), 1);
                this->set_count_ += n - old_size;
            }

            template <std::integral IntType>
            [[nodiscard]] constexpr bool contains(IntType x) const noexcept {
                if constexpr (std::signed_integral<IntType>) {
                    return x >= 0 && static_cast<size_type>(x) < this->parents_.size();
                } else {
                    return x < this->parents_.size();
                }
            }

            [[nodiscard]] constexpr size_type set_size(value_type x) const noexcept
            requires TrackSize {
                if (!this->contains(x)) {
                    return 0U;
                }
                return this->sizes_[this->get_ensured_root(x)];
            }

            [[nodiscard]] constexpr bool is_root(const value_type& x) const
            noexcept(std::is_nothrow_invocable_v<key_equal, const value_type&, const value_type&>) {
                return this->contains(x) && this->key_equal_(x, this->parents_[x]);
            }

            constexpr void reset_all() noexcept {
                std::iota(this->parents_.begin(), this->parents_.end(), 0);
                if constexpr (TrackSize) {
                    std::fill(this->sizes_.begin(), this->sizes_.end(), 1);
                }
                this->set_count_ = this->parents_.size();
            }
        };

        template <typename T, bool TrackSize, typename ParentStorage, typename SizeStorage, typename KeyEqual>
        using uf_base = std::conditional_t<concepts::unordered_map<ParentStorage>,
                            map_uf_base_<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>,
                            array_uf_base_<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>>;
    }

        template <
            concepts::object T,
            bool TrackSize = true,
            detail::valid_parent_storage<T> ParentStorage = detail::default_parent_storage_t<T>,
            detail::valid_size_storage<T, TrackSize, ParentStorage> SizeStorage =
                    detail::default_size_storage_t<T, TrackSize, ParentStorage>,
            concepts::comparison_functor<T> KeyEqual = detail::default_key_equal_t<ParentStorage>>
        class basic_union_find final : public detail::uf_base<T, TrackSize, ParentStorage, SizeStorage, KeyEqual> {
            static_assert(std::is_copy_assignable_v<T>, "Copy assignment is required for path compression and union");

            using base_uf = detail::uf_base<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>;
        public:
            using parent_storage = typename base_uf::parent_storage;
            using size_storage = typename base_uf::size_storage;
            using value_type = T;
            using size_type = typename base_uf::size_type;
            using difference_type = typename parent_storage::difference_type;
            using container_type = ParentStorage;
            using pointer = T*;
            using const_pointer = const T*;
            using reference = T&;
            using const_reference = const T&;
            using iterator = typename ParentStorage::iterator;
            using const_iterator = typename ParentStorage::const_iterator;
            using reverse_iterator = std::reverse_iterator<iterator>;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;
            using key_equal = KeyEqual;

            /********************** STATIC METHODS ********************/

            static constexpr bool is_track_size() noexcept {
                return TrackSize;
            }

            /********************** CONSTRUCTORS **********************/

            constexpr basic_union_find() requires std::default_initializable<base_uf> = default;

            template <typename VTy>
            constexpr basic_union_find(std::initializer_list<VTy> il)
            requires std::constructible_from<base_uf, const VTy*, const VTy*>
            : base_uf(il.begin(), il.end()) {   }

            template <typename... Args>
            explicit constexpr basic_union_find(Args&& ...args)
            requires (sizeof...(Args) > 0) &&
                     (!std::same_as<basic_union_find, std::remove_cvref_t<Args>> && ...) &&
                     std::constructible_from<base_uf, Args&&...>
            : base_uf{ std::forward<Args>(args)... } {   }

            constexpr basic_union_find(const basic_union_find&) = default;

            constexpr basic_union_find(basic_union_find&&)
            noexcept(std::is_nothrow_move_constructible_v<base_uf>) = default;

            constexpr basic_union_find& operator=(const basic_union_find&) = default;

            constexpr basic_union_find& operator=(basic_union_find&&)
            noexcept(std::is_nothrow_move_assignable_v<base_uf>) = default;

            constexpr ~basic_union_find() = default;

            /********************** CORE METHODS **********************/

            /**
             * @brief Insert if the element does not exist, then find its root with path compression.
             * @warning: If T is unsigned integral, do NOT pass any negative number into this method.
             * Otherwise, a resize to a massive size will be triggered.
             */
            [[nodiscard]] constexpr const_reference find_or_insert(const_reference x) {
                if (!this->contains(x)) {
                    if constexpr (concepts::unordered_map<parent_storage>) {
                        this->try_emplace(x);
                    } else {
                        this->resize(x + 1);
                    }
                }
                return this->get_ensured_root_and_compress(x);
            }

            // UB if the element does not exist
            [[nodiscard]] constexpr const_reference unchecked_find(const_reference x)
            noexcept(noexcept(this->get_ensured_root_and_compress(x))) {
                return this->get_ensured_root_and_compress(x);
            }

            [[nodiscard]] constexpr const_reference unchecked_find(const_reference x) const
            noexcept(noexcept(this->get_ensured_root(x))) {
                return this->get_ensured_root(x);
            }

            [[nodiscard]] constexpr bool same_set(const_reference x, const_reference y)
            noexcept(noexcept(this->get_ensured_root_and_compress(x)) &&
                     std::is_nothrow_invocable_v<key_equal, const_reference, const_reference>) {
                return this->contains(x) && this->contains(y) &&
                       this->key_equal_(this->get_ensured_root_and_compress(x),
                                       this->get_ensured_root_and_compress(y));
            }

            [[nodiscard]] constexpr bool same_set(const_reference x, const_reference y) const
            noexcept(noexcept(this->get_ensured_root(x)) &&
                     std::is_nothrow_invocable_v<key_equal, const_reference, const_reference>) {
                return this->contains(x) && this->contains(y) &&
                       this->key_equal_(this->get_ensured_root(x), this->get_ensured_root(y));
            }

            constexpr void unite_new(const_reference x, const_reference y) {
                this->unite_existing_roots(this->find_or_insert(x), this->find_or_insert(y));
            }

            constexpr bool try_unite(const_reference x, const_reference y)
            noexcept(noexcept(this->get_ensured_root_and_compress(x)) &&
                     noexcept(this->unite_existing_roots(x, y))) {
                if (!this->contains(x) || !this->contains(y)) {
                    return false;
                }
                this->unite_existing_roots(this->get_ensured_root_and_compress(x),
                                           this->get_ensured_root_and_compress(y));
                return true;
            }

            // UB if the element does not exist
            constexpr void unchecked_unite(const_reference x, const_reference y)
            noexcept(noexcept(this->get_ensured_root_and_compress(x)) &&
                     noexcept(this->unite_existing_roots(x, y))) {
                this->unite_existing_roots(this->get_ensured_root_and_compress(x),
                                           this->get_ensured_root_and_compress(y));
            }

            [[nodiscard]] constexpr size_type set_count() const noexcept {
                return this->set_count_;
            }

            /********************** CAPACITY **********************/

            [[nodiscard]] constexpr bool empty() const noexcept {
                return this->parents_.empty();
            }

            [[nodiscard]] constexpr size_type size() const noexcept {
                return this->parents_.size();
            }

            [[nodiscard]] constexpr std::size_t ssize() const noexcept {
                return static_cast<std::size_t>(this->size());
            }

            [[nodiscard]] constexpr size_type max_size() const noexcept {
                return this->parents_.max_size();
            }

            [[nodiscard]] constexpr size_type capacity() const noexcept
            requires requires() { { this->parents_.capacity() } -> std::convertible_to<size_type>; } {
                return this->parents_.capacity();
            }

            constexpr void reserve(const size_type n)
            requires (concepts::reservable_container<parent_storage>) {
                this->parents_.reserve(n);
                if constexpr (TrackSize && concepts::reservable_container<size_storage>) {
                    this->sizes_.reserve(n);
                }
            }

            /********************** ITERATORS **********************/
            
            [[nodiscard]] const_iterator begin() const noexcept {
                return this->parents_.begin();
            }

            [[nodiscard]] const_iterator end() const noexcept {
                return this->parents_.end();
            }

            [[nodiscard]] const_reverse_iterator rbegin() const noexcept {
                return std::reverse_iterator{ this->begin() };
            }

            [[nodiscard]] const_reverse_iterator rend() const noexcept {
                return std::reverse_iterator{ this->end() };
            }

            [[nodiscard]] const_iterator cbegin() const noexcept {
                return this->parents_.cbegin();
            }

            [[nodiscard]] const_iterator cend() const noexcept {
                return this->parents_.cend();
            }

            [[nodiscard]] const_reverse_iterator crbegin() const noexcept {
                return std::reverse_iterator{ this->cend() };
            }

            [[nodiscard]] const_reverse_iterator crend() const noexcept {
                return std::reverse_iterator{ this->cbegin() };
            }

            /********************** UTILITIES **********************/

            constexpr const parent_storage& get_parent_storage() const noexcept {
                return this->parents_;
            }

            constexpr const size_storage& get_size_storage() const noexcept
            requires TrackSize {
                return this->sizes_;
            }

            constexpr const key_equal& get_key_equal() const noexcept {
                return this->key_equal_;
            }

            constexpr void clear() noexcept
            requires (std::is_empty_v<key_equal> || requires { this->key_equal_.clear(); }) {
                this->parents_.clear();
                if (TrackSize) {
                    this->sizes_.clear();
                }
                if constexpr (std::is_empty_v<key_equal>) {
                    this->key_equal_.clear();
                }
                this->set_count_ = 0U;
            }

            constexpr void swap(basic_union_find& other)
            noexcept(std::is_nothrow_swappable_v<parent_storage> &&
                     std::is_nothrow_swappable_v<size_storage> &&
                     (std::is_empty_v<key_equal> || std::is_nothrow_swappable_v<key_equal>))
            requires (std::is_empty_v<key_equal> || std::swappable<key_equal>) {
                using std::swap;
                swap(this->parents_, other.parents_);
                if constexpr (TrackSize) {
                    swap(this->sizes_, other.sizes_);
                }
                if constexpr (!std::is_empty_v<key_equal>) {
                    swap(this->key_equal_, other.key_equal_);
                }
                swap(this->set_count_, other.set_count_);
            }

            [[nodiscard]]
            friend constexpr bool operator==(const basic_union_find& lhs, const basic_union_find& rhs) noexcept
            requires std::equality_comparable<parent_storage> {
                return lhs.set_count_ == rhs.set_count_ && lhs.parents_ == rhs.parents_;
            }
        };

        /********************** CTAD GUIDEs **********************/

    // From unsigned integral T
    template <std::integral T>
    basic_union_find(T n) -> basic_union_find<T>;

    // From unsigned integral T and a custom comparator
    template <std::integral T, concepts::comparison_functor<T> Comp>
    basic_union_find(T n, Comp) ->
        basic_union_find<T,
                         true,  // Tracks size
                         detail::default_parent_storage_t<T>,
                         detail::default_size_storage_t<T, true, detail::default_parent_storage_t<T>>,
                         Comp>;

    // From initializer list
    template <typename T>
    basic_union_find(std::initializer_list<T>) ->
        basic_union_find<T, true, std::unordered_map<T, T>, std::unordered_map<T, size_t>>;

    // From an iterator pair
    template <typename Iter>
    basic_union_find(Iter, Iter) ->
        basic_union_find<std::iter_value_t<Iter>,
                         true,
                         std::unordered_map<std::iter_value_t<Iter>, std::iter_value_t<Iter>>,
                         std::unordered_map<std::iter_value_t<Iter>, size_t>>;

    // From an iterator pair and a custom comparator
    template <typename Iter, concepts::comparison_functor<std::iter_value_t<Iter>> Comp>
    basic_union_find(Iter, Iter, Comp) ->
        basic_union_find<std::iter_value_t<Iter>,
                         true,
                         std::unordered_map<std::iter_value_t<Iter>, std::iter_value_t<Iter>>,
                         std::unordered_map<std::iter_value_t<Iter>, size_t>,
                         Comp>;

    // From a range
    template <typename Rng>
    basic_union_find(Rng&&) ->
        basic_union_find<std::ranges::range_value_t<Rng>,
                     true,
                     std::unordered_map<std::ranges::range_value_t<Rng>, std::ranges::range_value_t<Rng>>,
                     std::unordered_map<std::ranges::range_value_t<Rng>, size_t>>;


    template<typename T, bool TrackSize, typename ParentStorage, typename SizeStorage, typename KeyEqual>
    basic_union_find(const basic_union_find<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>&)
        -> basic_union_find<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>;


    template<typename T, bool TrackSize, typename ParentStorage, typename SizeStorage, typename KeyEqual>
    basic_union_find(basic_union_find<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>&&)
        -> basic_union_find<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>;

    // From a range and a comparator
    template <typename Rng, typename Comp>
    basic_union_find(Rng&&, Comp) ->
        basic_union_find<std::ranges::range_value_t<Rng>,
                         true,
                         std::unordered_map<std::ranges::range_value_t<Rng>, std::ranges::range_value_t<Rng>>,
                         std::unordered_map<std::ranges::range_value_t<Rng>, size_t>,
                         Comp>;


    /********************** TYPE ALIAS **********************/


    using union_find = basic_union_find<uint32_t, true, std::vector<uint32_t>, std::vector<uint32_t>>;

    // micro_union_find -- up to 256 elements, no size tracking
    using micro_union_find = basic_union_find<uint8_t, false>;

    template <std::size_t N, bool TrackSize = true>
    using static_union_find = basic_union_find<uint16_t, TrackSize,
                                    inplace_vector<uint16_t, N>, inplace_vector<uint16_t, N>>;

    template <typename T>
    using map_union_find = basic_union_find<T, true, std::unordered_map<T, T>, std::unordered_map<T, size_t>>;

    /********************** Identity **********************/

    namespace detail {
        template <typename Cont>
        struct is_basic_union_find {
            static constexpr bool value = false;
        };

        template <typename T, bool TrackSize, typename ParentStorage,
                  typename SizeStorage, typename KeyEqual>
        struct is_basic_union_find<
            basic_union_find<T, TrackSize, ParentStorage, SizeStorage, KeyEqual>> {
            static constexpr bool value = true;
        };
    }

    template <typename Cont>
    inline constexpr bool is_basic_union_find_v = detail::is_basic_union_find<Cont>::value;

    namespace concepts {
        template <typename Cont>
        concept basic_union_find = is_basic_union_find_v<Cont>;
    }

}
#endif //URLICHT_UNION_FIND_H
