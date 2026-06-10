#ifndef URLICHT_FLAT_MAP_H
#define URLICHT_FLAT_MAP_H

#include <urlicht/config.h>
#include <urlicht/concepts_utility.h>
#include <urlicht/algorithm/lower_bound.h>
#include <urlicht/adaptor/detail/flat_map_utils.h>
#include <urlicht/internal/tag.h>
#include <urlicht/internal/scope_guard.h>
#include <concepts>
#include <vector>
#include <initializer_list>


namespace urlicht {
    /**
     * @brief A sorted associative containers that stores key-value pairs with SoA layout (parallel key/value storage).
     *        It implements most methods of std::flat_map with the following extensions/modifications:
     *
     *        1. Template-configurable lower bound algorithm, with the default being more efficient then
     *           std::lower_bound. See benchmarks/algorithm/bm_lower_bound.cpp. Configurable algorithm allows for
     *           scenario-specifc optimizations (for examples, using linear search with small containers).
     *        2. Sorted tag (in addition to sorted_unique) for constructors and bulk insertion methods.
     *        3. Hardened static concept constraints for transparent look-up in heterogeneous overloads.
     *        4. Removed allocator parameter in constructors and discarded C++23 uses_allocator specialization.
     *        5. C++20 compatibility. Note that due to language limitations, flat_map::[const_]iterator does not
     *           satisfy iterator concepts until C++23.
     *
     * @tparam KeyType The type of keys stored. Must satisfy std::is_object_v.
     * @tparam MappedType The type of mapped values stored. Must satisfy std::is_object_v.
     * @tparam KeyCompare Comparison functor used to order keys. Must satisfy std::strict_weak_order<KeyCompare,
     *          KeyType, KeyType>. Defaults to std::less<>.
     * @tparam KeyContainer Standard-compliant contiguous container used as the key storage. Its value_type member
     *          type must be KeyType. Defaults to std::vector<KeyType>.
     * @tparam MappedContainer Standard-compliant contiguous container used as the value storage. Its value_type
     *          member type must be MappedType. Defaults to std::vector<MappedType>.
     * @tparam LowerBoundFn A functor used to perform lower_bound search on KeyContainer. It must be callable
     *          with parameters [const KeyContainer&, const KeyType&, KeyCompare] and return KeyContainer::iterator.
     */
    template <
        typename KeyType,
        typename MappedType,
        typename KeyCompare = std::less<>,
        typename KeyContainer = std::vector<KeyType>,
        typename MappedContainer = std::vector<MappedType>,
        typename LowerBoundFn = lower_bound_fn>
    class flat_map {
        // Type requirements
        static_assert(concepts::object<KeyType>, "KeyType must satisfy std::is_object_v");
        static_assert(concepts::object<MappedType>, "MappedType must satisfy std::is_object_v");
        static_assert(std::strict_weak_order<KeyCompare, KeyType, KeyType>);
        static_assert(concepts::contiguous_container<KeyContainer>);
        static_assert(concepts::contiguous_container<MappedContainer>);
        static_assert(adaptor_detail::lower_bound_for<LowerBoundFn, KeyType, KeyContainer, KeyCompare>);
        static_assert(std::same_as<KeyType, typename KeyContainer::value_type>);
        static_assert(std::same_as<MappedType, typename MappedContainer::value_type>);

        struct containers_set {
            KeyContainer key_container_;
            MappedContainer mapped_container_;
        };
    public:
        using key_container_type = KeyContainer;
        using mapped_container_type = MappedContainer;
        using key_type = KeyType;
        using mapped_type = MappedType;
        using value_type = std::pair<const key_type, mapped_type>;
        using key_compare = KeyCompare;
        using reference = std::pair<const key_type&, mapped_type&>;
        using const_reference = std::pair<const key_type&, const mapped_type&>;
        using size_type = std::size_t;
        using difference_type = std::ptrdiff_t;
        using containers = containers_set;
        using lower_bound_function_type = LowerBoundFn;
    private:
        using movable_value_type = std::pair<key_type, mapped_type>;
        using key_iterator = typename key_container_type::const_iterator;
        using mapped_iterator = typename mapped_container_type::iterator;
        using const_mapped_iterator = typename mapped_container_type::const_iterator;

        template <typename Ref>
            class proxy_for_arrow {
            Ref ref_;
        public:
            constexpr proxy_for_arrow(Ref ref) noexcept : ref_{ref} {}
            constexpr auto operator->() const noexcept { return &ref_; }
        };

        template <bool IsConst>
        class iterator_impl {
            using self_type = iterator_impl;
            using _key_iterator = key_iterator;
            using _mapped_iterator = std::conditional_t<IsConst, const_mapped_iterator, mapped_iterator>;
        public:
            using iterator_category = std::random_access_iterator_tag;
            using value_type = flat_map::value_type;
            using difference_type = flat_map::difference_type;
            using reference = std::conditional_t<IsConst, flat_map::const_reference, flat_map::reference>;
            using pointer = proxy_for_arrow<reference>;

            template <bool> friend class iterator_impl;
            template <typename, typename, typename, typename, typename, typename> friend class flat_map;

            constexpr iterator_impl() noexcept = default;

            constexpr iterator_impl(_key_iterator key, _mapped_iterator mapped) noexcept
                : key_{key}, mapped_{mapped} { }

            template <bool OtherConst>
            requires IsConst && (!OtherConst)
            constexpr iterator_impl(const iterator_impl<OtherConst>& other) noexcept
                : key_{other.key_}, mapped_{other.mapped_} {}

            constexpr iterator_impl(const self_type&) noexcept = default;
            constexpr iterator_impl(self_type&&) noexcept = default;
            constexpr iterator_impl& operator=(const self_type&) noexcept = default;
            constexpr iterator_impl& operator=(self_type&&) noexcept = default;
            constexpr ~iterator_impl() noexcept = default;

            constexpr reference operator*() const noexcept {
                return {*key_, *mapped_};
            }

            constexpr auto operator->() const noexcept {
                return pointer{reference{*key_, *mapped_}};
            }

            constexpr reference operator[](difference_type index) const noexcept {
                return *(*this + index);
            }

            constexpr self_type& operator++() noexcept {
                ++key_;
                ++mapped_;
                return *this;
            }

            constexpr self_type operator++(int) noexcept {
                auto tmp{*this};
                ++*this;
                return tmp;
            }

            constexpr self_type& operator--() noexcept {
                --key_;
                --mapped_;
                return *this;
            }

            constexpr self_type operator--(int) noexcept {
                auto tmp{*this};
                --*this;
                return tmp;
            }

            constexpr self_type& operator+=(difference_type diff) noexcept {
                key_ += diff;
                mapped_ += diff;
                return *this;
            }

            constexpr self_type& operator-=(difference_type diff) noexcept {
                key_ -= diff;
                mapped_ -= diff;
                return *this;
            }

            constexpr self_type operator+(difference_type index) const noexcept {
                return {key_ + index, mapped_ + index};
            }

            constexpr self_type operator-(difference_type index) const noexcept {
                return {key_ - index, mapped_ - index};
            }

            friend constexpr self_type operator+(difference_type index, const self_type& rhs) noexcept {
                return rhs + index;
            }

            template <bool RhsConst>
            friend constexpr difference_type
            operator-(const self_type& lhs, const iterator_impl<RhsConst>& rhs) noexcept {
                return lhs.key_ - rhs.key_;
            }

            template <bool RhsConst>
            friend constexpr auto operator==(const self_type& lhs, const iterator_impl<RhsConst>& rhs) noexcept {
                return lhs.key_ == rhs.key_;
            }

            template <bool RhsConst>
            friend constexpr auto operator<=>(const self_type& lhs, const iterator_impl<RhsConst>& rhs) noexcept
            requires std::three_way_comparable<_key_iterator> {
                return lhs.key_ <=> rhs.key_;
            }

        private:
            _key_iterator key_{};
            _mapped_iterator mapped_{};

            constexpr std::pair<_key_iterator, _mapped_iterator> key_and_mapped_iter_() const noexcept {
                return std::make_pair(key_, mapped_);
            }
        };
    public:
        /**
         * @warning: Due to language limitations, this will not satisfy any iterator concepts until C++23.
         */
        using iterator = iterator_impl<false>;
        /**
         * @warning: Due to language limitations, this will not satisfy any iterator concepts until C++23.
         */
        using const_iterator = iterator_impl<true>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    private:
        //************************ Core utility methods ************************//


        static constexpr bool is_comp_transparent_v = requires { typename key_compare::is_transparent; } ;

        template <typename Key>
        static constexpr bool is_valid_key_v =
            std::same_as<std::remove_cvref_t<Key>, key_type>
        || (is_comp_transparent_v && std::strict_weak_order<key_compare, std::remove_cvref_t<Key>, key_type>);

        constexpr key_container_type& keys_mut_() noexcept { return this->containers_.key_container_; }
        constexpr mapped_container_type& values_mut_() noexcept { return this->containers_.mapped_container_; }

        constexpr mapped_iterator corresponding_mapped_iter_(key_iterator key_it) noexcept {
            return this->values_mut_().begin() +
                static_cast<std::ranges::range_difference_t<mapped_container_type>>(key_it - this->keys().cbegin());
        }

        constexpr const_mapped_iterator corresponding_mapped_iter_(key_iterator key_it) const noexcept {
            return this->values().cbegin() +
                static_cast<std::ranges::range_difference_t<mapped_container_type>>(key_it - this->keys().cbegin());
        }

        template <typename Key>
        constexpr key_iterator key_lower_bound_(const Key& key) const {
            return this->lower_bound_fn_(this->keys(), key, this->comp_);
        }

        // Note: key_it must be a result of lower_bound
        template <typename Key>
        constexpr bool key_iter_points_to_(key_iterator key_it, const Key& key) const noexcept {
            return key_it != this->keys().end() && !this->comp_(key, *key_it);
        }

        template <typename Key, typename ...VArgs>
        requires is_valid_key_v<Key>
        constexpr std::pair<iterator, bool> emplace_at_pos_(const_iterator pos, Key&& key, VArgs&&... vargs) {
            auto offset = pos - this->cbegin();
            auto [key_pos, mapped_pos] = pos.key_and_mapped_iter_();
            try {
                this->keys_mut_().emplace(key_pos, std::forward<Key>(key));
            } catch (...) {
                if constexpr (!adaptor_detail::emplace_should_have_strong_exception_safety_for<key_type, Key>) {
                    this->clear();
                }
                throw;
            }
            try {
                this->values_mut_().emplace(mapped_pos, std::forward<VArgs>(vargs)...);
            } catch (...) {
                if constexpr (adaptor_detail::emplace_should_have_strong_exception_safety_for<mapped_type, VArgs...>) {
                    try {
                        this->keys_mut_().erase(this->keys_mut_().begin() + offset);
                    } catch (...) {
                        this->clear();
                    }
                } else {
                    this->clear();
                }
                throw;
            }
            return std::make_pair(this->begin() + offset, true);
        }

        /**
         * @return 0: wrong position   1: exists at hinted position    2: ok to emplace
         */
        template <typename Key>
        constexpr int is_hint_correct_(const_iterator hint, const Key& key) const {
            if (hint != this->end()) [[likely]] {
                if (this->comp_(hint->first, key)) {
                    return 0;
                }
                if (!this->comp_(key, hint->first)) {
                    return 1;
                }
            }
            if (hint != this->begin() && !this->comp_((hint - 1)->first, key)) {
                return 0;
            }
            return 2;
        }

        template <typename Key, typename ...VArgs>
        constexpr std::pair<iterator, bool> try_emplace_(Key&& key, VArgs&&... vargs) {
            iterator pos = lower_bound_impl_<iterator>(*this, key);
            if (pos != this->end() && this->key_iter_points_to_(pos.key_, key)) { // Already exists
                return std::make_pair(pos, false);
            }
            return this->emplace_at_pos_(pos, std::forward<Key>(key), std::forward<VArgs>(vargs)...);
        }

        template <typename Key, typename ...VArgs>
        constexpr std::pair<iterator, bool> try_emplace_hint_(const_iterator hint, Key&& key, VArgs&&... vargs) {
            switch (is_hint_correct_(hint, key)) {
                case 0: {
                    return try_emplace_(std::forward<Key>(key), std::forward<VArgs>(vargs)...);
                }
                case 1: {
                    auto offset = hint - this->cbegin();
                    return std::make_pair(this->begin() + offset, false);
                }
                default: /* case 2: */
                    return this->emplace_at_pos_(hint, std::forward<Key>(key), std::forward<VArgs>(vargs)...);
            }
        }

        template <typename Key, typename Mapped>
        constexpr std::pair<iterator, bool> insert_or_assign_impl_(Key&& key, Mapped&& mapped) {
            auto res = this->try_emplace_(std::forward<Key>(key), std::forward<Mapped>(mapped));
            if (res.second == false) {
                res.first->second = std::forward<Mapped>(mapped);
            }
            return res;
        }

        template <typename Key, typename Mapped>
        constexpr iterator insert_or_assign_impl_(const_iterator hint, Key&& key, Mapped&& mapped) {
            auto res = this->try_emplace_hint_(hint, std::forward<Key>(key), std::forward<Mapped>(mapped));
            if (res.second == false) {
                res.first->second = std::forward<Mapped>(mapped);
            }
            return res.first;
        }

        template <typename Ret, typename Self, typename Key>
        static constexpr Ret lower_bound_impl_(Self&& self, const Key& key) {
            key_iterator key_it = self.key_lower_bound_(key);
            return Ret{key_it, self.corresponding_mapped_iter_(key_it)};
        }

        template <typename Ret, typename Self, typename Key>
        static constexpr Ret upper_bound_impl_(Self&& self, const Key& key) {
            auto swapped_reverse_comp_ = [comp = self.comp_](const auto& lhs, const auto& rhs) {
                return !comp(rhs, lhs);
            };
            key_iterator key_it = self.lower_bound_fn_(self.keys(), key, swapped_reverse_comp_);
            return Ret{key_it, self.corresponding_mapped_iter_(key_it)};
        }

        template <typename Iter, typename Self, typename Key>
        static constexpr std::pair<Iter, Iter> equal_range_impl_(Self&& self, const Key& key) {
            Iter it = upper_bound_impl_<Iter>(std::forward<Self>(self), key);
            if (it != self.begin() && !self.comp_((it - 1)->first, key)) {
                return std::make_pair(it - 1, it);
            }
            return std::make_pair(it, it);
        }

        template <typename Self, typename Key>
        static constexpr auto find_impl_(Self&& self, const Key& key) {
            key_iterator key_it = self.key_lower_bound_(key);
            if (!self.key_iter_points_to_(key_it, key)) [[unlikely]] {
                return self.end();
            }
            return decltype(self.begin()){key_it, self.corresponding_mapped_iter_(key_it)};
        }

        template <typename Self, typename Key>
        static constexpr decltype(auto) at_impl_(Self&& self, const Key& key, const char* msg) {
            key_iterator key_it = self.key_lower_bound_(key);
            if (!self.key_iter_points_to_(key_it, key)) [[unlikely]] {
                throw std::out_of_range(msg);
            }
            return *self.corresponding_mapped_iter_(key_it);
        }

        template <typename Iter>
        constexpr iterator erase_impl_(Iter pos) {
            auto [key_pos, value_pos] = pos.key_and_mapped_iter_();
            try {
                auto key_pos_after = this->keys_mut_().erase(key_pos);
                auto value_pos_after = this->values_mut_().erase(value_pos);
                return iterator{key_pos_after, value_pos_after};
            } catch (...) {
                this->clear();
                throw;
            }
        }

        template <typename Key>
        constexpr size_type erase_key_impl_(const Key& key) {
            auto it = this->lower_bound(key);
            if (it == this->end() || !this->key_iter_points_to_(it.key_, key)) [[unlikely]] {
                return 0;
            }
            this->erase_impl_(it);
            return 1;
        }

        template <typename Rng>
        constexpr size_type append_range_(Rng&& rng) {
            size_type append_size = 0U;
            for (auto&& v : rng) {
                movable_value_type p{std::forward<decltype(v)>(v)};
                this->keys_mut_().emplace_back(std::move(p.first));
                this->values_mut_().emplace_back(std::move(p.second));
                ++append_size;
            }
            return append_size;
        }

        // Usages: 1. Append unsorted range: <true, true>
        //         2. Sort and make unique current arrays (for ctors): <false, true>
        //         3. Append sorted range: <true, false>
        //         4. Make unique current arrays (for ctors): <false, false>
        template <bool ToAppend, bool ToSort, typename Rng>
        constexpr void sort_unique_append_([[maybe_unused]] Rng&& rng) {
            try {
                [[maybe_unused]] difference_type offset = 0;
                // Append
                if constexpr (ToAppend) {
                    auto append_size = this->append_range_(std::forward<Rng>(rng));
                    offset = this->size() - append_size; // First inserted elem
                }
                [[maybe_unused]] auto cmp_key = [&](const auto& lhs, const auto& rhs) -> bool {
                    return this->comp_(lhs.first, rhs.first);
                };
                // Sort
                adaptor_detail::zip_view<key_container_type, mapped_container_type> zv{
                    this->keys_mut_(), this->values_mut_()
                };
                [[maybe_unused]] auto zv_mid = zv.begin() + offset;
                if constexpr (ToSort) {
                    std::ranges::sort(zv_mid, zv.end(), cmp_key);
                }
                if constexpr (ToAppend) {
                    std::ranges::inplace_merge(zv.begin(), zv_mid, zv.end(), cmp_key);
                }
                // Unique
                auto key_eq = [&](const auto& lhs, const auto& rhs) -> bool {
                    return !this->comp_(lhs.first, rhs.first);
                };
                auto dup_rng = std::ranges::unique(zv, key_eq);
                auto dup_offset = dup_rng.begin() - zv.begin();
                this->keys_mut_().erase(this->keys_mut_().begin() + dup_offset, this->keys_mut_().end());
                this->values_mut_().erase(this->values_mut_().begin() + dup_offset, this->values_mut_().end());
            } catch (...) {
                this->clear();
                throw;
            }
        }

        // Data members
        containers containers_{};
        UL_NO_UNIQUE_ADDRESS key_compare comp_{};
        UL_NO_UNIQUE_ADDRESS lower_bound_function_type lower_bound_fn_{};

    public:
        // Default constructor
        constexpr flat_map()
        noexcept(std::is_nothrow_default_constructible_v<containers> &&
                 std::is_nothrow_default_constructible_v<key_compare> &&
                 std::is_nothrow_default_constructible_v<lower_bound_function_type>)
        = default;

        template <concepts::can_construct<key_compare> KeyComp_>
        explicit constexpr flat_map(KeyComp_&& key_comp)
        noexcept(std::is_nothrow_default_constructible_v<containers> &&
                 std::is_nothrow_constructible_v<key_compare, KeyComp_&&> &&
                 std::is_nothrow_default_constructible_v<lower_bound_function_type>)
        : comp_{std::forward<KeyComp_>(key_comp)} { }

        template <concepts::can_construct<key_compare> KeyComp_,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_>
        constexpr flat_map(KeyComp_&& key_comp, LowerBoundFn_&& lower_bound)
        noexcept(std::is_nothrow_default_constructible_v<containers> &&
                 std::is_nothrow_constructible_v<key_compare, KeyComp_&&> &&
                 std::is_nothrow_constructible_v<lower_bound_function_type, LowerBoundFn_&&>)
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} { }

        template <concepts::can_construct<key_container_type> KeyCont_,
                  concepts::can_construct<mapped_container_type> MappedCont_,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(KeyCont_&& key_cont, MappedCont_&& mapped_cont,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : containers_{std::forward<KeyCont_>(key_cont), std::forward<MappedCont_>(mapped_cont)},
          comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT(this->keys().size() == this->values().size(), "Different sizes of key/mapped");
            sort_unique_append_<false, true>(0U);   // Sort unique
        }

        template <concepts::can_construct<key_container_type> KeyCont_,
                  concepts::can_construct<mapped_container_type> MappedCont_,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_t,
                           KeyCont_&& key_cont, MappedCont_&& mapped_cont,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : containers_{std::forward<KeyCont_>(key_cont), std::forward<MappedCont_>(mapped_cont)},
          comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT(this->keys().size() == this->values().size(), "Different sizes of key/mapped");
            UL_ASSERT((std::ranges::is_sorted(this->keys(), this->comp_)), "Keys are not sorted");
            sort_unique_append_<false, false>(0U);  // Unique (v.) only
        }

        template <concepts::can_construct<key_container_type> KeyCont_,
                  concepts::can_construct<mapped_container_type> MappedCont_,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_unique_t,
                           KeyCont_&& key_cont, MappedCont_&& mapped_cont,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : containers_{std::forward<KeyCont_>(key_cont), std::forward<MappedCont_>(mapped_cont)},
          comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT(this->keys().size() == this->values().size(), "Different sizes of key/mapped");
            UL_ASSERT((adaptor_detail::is_sorted_and_unique(this->keys().begin(), this->keys().end(), this->comp_)),
                "Keys are not sorted unique");
        }

        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(Iter first, Sentinel last,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            sort_unique_append_<true, true>(std::ranges::subrange(first, last));
        }

        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_t,
                           Iter first, Sentinel last,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT((std::ranges::is_sorted(first, last, this->value_comp())),
                      "Input range not sorted.");
            sort_unique_append_<true, false>(std::ranges::subrange(first, last));
        }

        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_unique_t,
                           Iter first, Sentinel last,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT((adaptor_detail::is_sorted_and_unique(first, last, this->value_comp())),
                      "Input range not sorted unique.");
            append_range_(std::ranges::subrange(first, last));
        }

        template <concepts::compatible_range<value_type> Rng,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        requires (!std::same_as<std::remove_cvref_t<Rng>, flat_map>)
        constexpr flat_map(Rng&& rng,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            sort_unique_append_<true, true>(std::forward<Rng>(rng));
        }

        template <concepts::compatible_range<value_type> Rng,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_t,
                           Rng&& rng,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT((std::ranges::is_sorted(rng, this->value_comp())), "Input range not sorted.");
            sort_unique_append_<true, false>(std::forward<Rng>(rng));
        }

        template <concepts::compatible_range<value_type> Rng,
                  concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_unique_t,
                           Rng&& rng,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : comp_{std::forward<KeyComp_>(key_comp)},
          lower_bound_fn_{std::forward<LowerBoundFn_>(lower_bound)} {
            UL_ASSERT((adaptor_detail::is_sorted_and_unique(
                            std::ranges::begin(rng), std::ranges::end(rng), this->value_comp())),
                      "Input range not sorted unique.");
            append_range_(std::forward<Rng>(rng));
        }

        template <concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(std::initializer_list<value_type> init,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : flat_map(init.begin(), init.end(),
                   std::forward<KeyComp_>(key_comp),
                   std::forward<LowerBoundFn_>(lower_bound)) {}

        template <concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_t,
                           std::initializer_list<value_type> init,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : flat_map(sorted, init.begin(), init.end(),
                   std::forward<KeyComp_>(key_comp),
                   std::forward<LowerBoundFn_>(lower_bound)) {}

        template <concepts::can_construct<key_compare> KeyComp_ = key_compare,
                  concepts::can_construct<lower_bound_function_type> LowerBoundFn_ = lower_bound_function_type>
        constexpr flat_map(detail::sorted_unique_t,
                           std::initializer_list<value_type> init,
                           KeyComp_&& key_comp = KeyComp_{},
                           LowerBoundFn_&& lower_bound = LowerBoundFn_{})
        : flat_map(sorted_unique, init.begin(), init.end(),
                   std::forward<KeyComp_>(key_comp),
                   std::forward<LowerBoundFn_>(lower_bound)) {}

        constexpr flat_map(const flat_map&) = default;

        constexpr flat_map(flat_map&& other)
        noexcept(std::is_nothrow_move_constructible_v<containers> &&
                 std::is_nothrow_move_constructible_v<key_compare> &&
                 std::is_nothrow_move_constructible_v<lower_bound_function_type>)
        try : containers_(std::move(other.containers_)),
              comp_(std::move(other.comp_)),
              lower_bound_fn_(std::move(other.lower_bound_fn_)) {
            other.clear();
        } catch (...) {
            other.clear();
            throw;
        }

        constexpr flat_map& operator=(const flat_map& other) {
            if (this == &other) [[unlikely]] {
                return *this;
            }
            try {
                this->containers_ = other.containers_;
                this->comp_ = other.comp_;
                this->lower_bound_fn_ = other.lower_bound_fn_;
            } catch (...) {
                this->clear();
                throw;
            }
            return *this;
        }

        constexpr flat_map& operator=(flat_map&& other)
        noexcept(std::is_nothrow_move_assignable_v<containers> &&
                 std::is_nothrow_move_assignable_v<key_compare> &&
                 std::is_nothrow_move_assignable_v<lower_bound_function_type>) {
            if (this == &other) [[unlikely]] {
                return *this;
            }
            auto clear_other_guard = detail::make_scope_guard([&]() noexcept { other.clear(); });
            // Using try-catch block in a conditionally noexcept function causes warning in some compiler
            if constexpr (std::is_nothrow_move_constructible_v<flat_map>) {
                this->containers_ = std::move(other.containers_);
                this->comp_ = std::move(other.comp_);
                this->lower_bound_fn_ = std::move(other.lower_bound_fn_);
            } else {
                auto clear_this_guard = detail::make_scope_guard([&]() noexcept { this->clear(); });
                this->containers_ = std::move(other.containers_);
                this->comp_ = std::move(other.comp_);
                this->lower_bound_fn_ = std::move(other.lower_bound_fn_);
                clear_this_guard.release();
            }
            return *this;
        }

        constexpr flat_map& operator=(std::initializer_list<value_type> init) {
            this->clear();
            sort_unique_append_<true, true>(init);
            return *this;
        }

        //************************* Observers *************************//

        [[nodiscard]] constexpr const key_container_type& keys() const noexcept {
            return this->containers_.key_container_;
        }

        [[nodiscard]] constexpr const mapped_container_type& values() const noexcept {
            return this->containers_.mapped_container_;
        }

        [[nodiscard]] constexpr const key_compare& key_comp() const noexcept {
            return this->comp_;
        }

        [[nodiscard]] constexpr auto value_comp() const noexcept {
            return [comp = this->comp_](const value_type& lhs, const value_type& rhs)
            noexcept(concepts::nothrow_invocable<key_compare, const key_type&, const key_type&>) {
                return comp(lhs.first, rhs.first);
            };
        }

        [[nodiscard]] constexpr auto lower_bound_fn() const noexcept {
            return this->lower_bound_fn_;
        }

        //************************* Iterators *************************//

        constexpr iterator begin() noexcept {
            return {this->keys_mut_().begin(), this->values_mut_().begin()};
        }
        constexpr const_iterator begin() const noexcept {
            return {this->keys().cbegin(), this->values().cbegin()};
        }
        constexpr const_iterator cbegin() const noexcept {
            return this->begin();
        }

        constexpr reverse_iterator rbegin() noexcept {
            return reverse_iterator(this->end());
        }
        constexpr const_reverse_iterator rbegin() const noexcept {
            return const_reverse_iterator(this->end());
        }
        constexpr const_reverse_iterator crbegin() const noexcept {
            return const_reverse_iterator(this->cend());
        }

        constexpr iterator end() noexcept {
            return {this->keys_mut_().end(), this->values_mut_().end()};
        }
        constexpr const_iterator end() const noexcept {
            return {this->keys().cend(), this->values().cend()};
        }
        constexpr const_iterator cend() const noexcept {
            return this->end();
        }

        constexpr reverse_iterator rend() noexcept {
            return reverse_iterator(this->begin());
        }
        constexpr const_reverse_iterator rend() const noexcept {
            return const_reverse_iterator(this->begin());
        }
        constexpr const_reverse_iterator crend() const noexcept {
            return const_reverse_iterator(this->cbegin());
        }

        constexpr iterator nth(size_type n) noexcept {
            UL_ASSERT(n < this->keys().size(), "flat_map::nth(): out-of-bound");
            return this->begin() + n;
        }

        constexpr const_iterator nth(size_type n) const noexcept {
            UL_ASSERT(n < this->keys().size(), "flat_map::nth() const: out-of-bound");
            return this->cbegin() + n;
        }

        constexpr difference_type index_of(iterator it) const noexcept {
            return it - this->begin();
        }

        constexpr difference_type index_of(const_iterator it) const noexcept {
            return it - this->begin();
        }

        //************************* Capacity *************************//

        [[nodiscard]] constexpr size_type size() const noexcept {
            return this->keys().size();
        }

        [[nodiscard]] constexpr bool empty() const noexcept {
            return this->keys().empty();
        }

        [[nodiscard]] constexpr size_type max_size() const noexcept {
            return std::min<size_type>(this->keys().max_size(), this->values().max_size());
        }

        constexpr void reserve(size_type n) {
            if constexpr (concepts::reservable_container<key_container_type>) {
                this->keys_mut_().reserve(n);
            }
            if constexpr (concepts::reservable_container<mapped_container_type>) {
                this->values_mut_().reserve(n);
            }
        }

        constexpr void shrink_to_fit() {
            if constexpr (concepts::reservable_container<key_container_type>) {
                this->keys_mut_().shrink_to_fit();
            }
            if constexpr (concepts::reservable_container<mapped_container_type>) {
                this->values_mut_().shrink_to_fit();
            }
        }

        //********************* Lookup & Element access **********************//

        [[nodiscard]] constexpr bool contains(const key_type& key) const {
            key_iterator key_it = this->key_lower_bound_(key);
            return this->key_iter_points_to_(key_it, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr bool contains(const Key& key) const {
            key_iterator key_it = this->key_lower_bound_(key);
            return this->key_iter_points_to_(key_it, key);
        }

        [[nodiscard]] constexpr size_type count(const key_type& key) const {
            return this->contains(key) ? 1u : 0u;
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr size_type count(const Key& key) const {
            return this->contains(key) ? 1u : 0u;
        }

        [[nodiscard]] constexpr iterator find(const key_type& key) {
            return find_impl_(*this, key);
        }

        [[nodiscard]] constexpr const_iterator find(const key_type& key) const {
            return find_impl_(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr iterator find(const Key& key) {
            return find_impl_(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr const_iterator find(const Key& key) const {
            return find_impl_(*this, key);
        }

        [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(const key_type& key) {
            return equal_range_impl_<iterator>(*this, key);
        }

        [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(const key_type& key) const {
            return equal_range_impl_<const_iterator>(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr std::pair<iterator, iterator> equal_range(const Key& key) {
            return equal_range_impl_<iterator>(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr std::pair<const_iterator, const_iterator> equal_range(const Key& key) const {
            return equal_range_impl_<const_iterator>(*this, key);
        }

        [[nodiscard]] constexpr iterator lower_bound(const key_type& key) {
            return lower_bound_impl_<iterator>(*this, key);
        }

        [[nodiscard]] constexpr const_iterator lower_bound(const key_type& key) const {
            return lower_bound_impl_<const_iterator>(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr iterator lower_bound(const Key& key) {
            return lower_bound_impl_<iterator>(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr const_iterator lower_bound(const Key& key) const {
            return lower_bound_impl_<const_iterator>(*this, key);
        }

        [[nodiscard]] constexpr iterator upper_bound(const key_type& key) {
            return upper_bound_impl_<iterator>(*this, key);
        }

        [[nodiscard]] constexpr const_iterator upper_bound(const key_type& key) const {
            return upper_bound_impl_<const_iterator>(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr iterator upper_bound(const Key& key) {
            return upper_bound_impl_<iterator>(*this, key);
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr const_iterator upper_bound(const Key& key) const {
            return upper_bound_impl_<const_iterator>(*this, key);
        }

        [[nodiscard]] constexpr mapped_type& at(const key_type& key) {
            return at_impl_(*this, key, "flat_map::at(): key not found");
        }

        [[nodiscard]] constexpr const mapped_type& at(const key_type& key) const {
            return at_impl_(*this, key, "flat_map::at() const: key not found");
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr mapped_type& at(const Key& key) {
            return at_impl_(*this, key, "flat_map::at(): key not found");
        }

        template <typename Key>
        requires is_valid_key_v<Key>
        [[nodiscard]] constexpr const mapped_type& at(const Key& key) const {
            return at_impl_(*this, key, "flat_map::at() const: key not found");
        }

        constexpr mapped_type& operator[](const key_type& key)
        requires std::default_initializable<mapped_type> {
            return this->try_emplace_(key).first->second;
        }

        constexpr mapped_type& operator[](key_type&& key)
        requires std::default_initializable<mapped_type> {
            return this->try_emplace_(std::move(key)).first->second;
        }

        template <typename Key>
        requires is_valid_key_v<Key> &&
                 std::constructible_from<key_type, Key&&> &&
                 std::default_initializable<mapped_type>
        constexpr mapped_type& operator[](Key&& key) {
            return this->try_emplace_(std::forward<Key>(key)).first->second;
        }

        //********************* Modifiers **********************//

        template <typename ...VArgs>
        requires std::constructible_from<mapped_type, VArgs&&...>
        constexpr std::pair<iterator, bool> try_emplace(const key_type& key, VArgs&& ...args) {
            return this->try_emplace_(key, std::forward<VArgs>(args)...);
        }

        template <typename ...VArgs>
        requires std::constructible_from<mapped_type, VArgs&&...>
        constexpr std::pair<iterator, bool> try_emplace(key_type&& key, VArgs&& ...args) {
            return this->try_emplace_(std::move(key), std::forward<VArgs>(args)...);
        }

        template <typename Key, typename ...VArgs>
        requires is_valid_key_v<Key> &&
                 std::constructible_from<key_type, Key&&> &&
                 std::constructible_from<mapped_type, VArgs&&...> &&
                 (!std::convertible_to<Key&&, const_iterator>) &&
                 (!std::convertible_to<Key&&, iterator>)
        constexpr std::pair<iterator, bool> try_emplace(Key&& key, VArgs&& ...args) {
            return this->try_emplace_(std::forward<Key>(key), std::forward<VArgs>(args)...);
        }

        template <typename ...VArgs>
        requires std::constructible_from<mapped_type, VArgs&&...>
        constexpr iterator try_emplace(const_iterator hint, const key_type& key, VArgs&& ...args) {
            return this->try_emplace_hint_(hint, key, std::forward<VArgs>(args)...).first;
        }

        template <typename ...VArgs>
        requires std::constructible_from<mapped_type, VArgs&&...>
        constexpr iterator try_emplace(const_iterator hint, key_type&& key, VArgs&& ...args) {
            return this->try_emplace_hint_(hint, std::move(key), std::forward<VArgs>(args)...).first;
        }

        template <typename Key, typename ...VArgs>
        requires is_valid_key_v<Key> &&
                 std::constructible_from<key_type, Key&&> &&
                 std::constructible_from<mapped_type, VArgs&&...>
        constexpr iterator try_emplace(const_iterator hint, Key&& key, VArgs&& ...args) {
            return this->try_emplace_hint_(hint, std::forward<Key>(key), std::forward<VArgs>(args)...).first;
        }

        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr std::pair<iterator, bool> emplace(Args&&... args) {
            movable_value_type pair{std::forward<Args>(args)...};
            return this->try_emplace_(std::move(pair.first), std::move(pair.second));
        }

        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr iterator emplace_hint(const_iterator hint, Args&&... args) {
            movable_value_type pair{std::forward<Args>(args)...};
            return this->try_emplace_hint_(hint, std::move(pair.first), std::move(pair.second)).first;
        }

        template <typename Mapped>
        requires std::constructible_from<mapped_type, Mapped&&> &&
                 std::assignable_from<mapped_type&, Mapped&&>
        constexpr std::pair<iterator, bool> insert_or_assign(const key_type& key, Mapped&& mapped) {
            return this->insert_or_assign_impl_(key, std::forward<Mapped>(mapped));
        }

        template <typename Mapped>
        requires std::constructible_from<mapped_type, Mapped&&> &&
                 std::assignable_from<mapped_type&, Mapped&&>
        constexpr std::pair<iterator, bool> insert_or_assign(key_type&& key, Mapped&& mapped) {
            return this->insert_or_assign_impl_(std::move(key), std::forward<Mapped>(mapped));
        }

        template <typename Key, typename Mapped>
        requires is_valid_key_v<Key> &&
                 std::constructible_from<key_type, Key&&> &&
                 std::constructible_from<mapped_type, Mapped&&> &&
                 std::assignable_from<mapped_type&, Mapped&&>
        constexpr std::pair<iterator, bool> insert_or_assign(Key&& key, Mapped&& mapped) {
            return this->insert_or_assign_impl_(std::forward<Key>(key), std::forward<Mapped>(mapped));
        }

        template <typename Mapped>
        requires std::constructible_from<mapped_type, Mapped&&> &&
                 std::assignable_from<mapped_type&, Mapped&&>
        constexpr iterator insert_or_assign(const_iterator hint, const key_type& key, Mapped&& mapped) {
            return this->insert_or_assign_impl_(hint, key, std::forward<Mapped>(mapped));
        }

        template <typename Mapped>
        requires std::constructible_from<mapped_type, Mapped&&> &&
                 std::assignable_from<mapped_type&, Mapped&&>
        constexpr iterator insert_or_assign(const_iterator hint, key_type&& key, Mapped&& mapped) {
            return this->insert_or_assign_impl_(hint, std::move(key), std::forward<Mapped>(mapped));
        }

        template <typename Key, typename Mapped>
        requires is_valid_key_v<Key> &&
                 std::constructible_from<key_type, Key&&> &&
                 std::constructible_from<mapped_type, Mapped&&> &&
                 std::assignable_from<mapped_type&, Mapped&&>
        constexpr iterator insert_or_assign(const_iterator hint, Key&& key, Mapped&& mapped) {
            return this->insert_or_assign_impl_(hint, std::forward<Key>(key), std::forward<Mapped>(mapped));
        }

        constexpr std::pair<iterator, bool> insert(const value_type& value) {
            return this->try_emplace_(value.first, value.second);
        }

        constexpr std::pair<iterator, bool> insert(value_type&& value) {
            return this->try_emplace_(std::move(value.first), std::move(value.second));
        }

        template <concepts::can_construct<value_type> PairLike>
        requires (!std::same_as<std::remove_cvref_t<PairLike>, value_type>)
        constexpr std::pair<iterator, bool> insert(PairLike&& value) {
            movable_value_type pair{std::forward<PairLike>(value)};
            return this->try_emplace_(std::move(pair.first), std::move(pair.second));
        }

        constexpr iterator insert(const_iterator hint, const value_type& value) {
            return this->try_emplace_hint_(hint, value.first, value.second).first;
        }

        constexpr iterator insert(const_iterator hint, value_type&& value) {
            return this->try_emplace_hint_(hint, std::move(value.first), std::move(value.second)).first;
        }

        template <concepts::can_construct<value_type> PairLike>
        requires (!std::same_as<std::remove_cvref_t<PairLike>, value_type>)
        constexpr iterator insert(const_iterator hint, PairLike&& value) {
            value_type pair{std::forward<PairLike>(value)};
            return this->try_emplace_hint_(hint, std::move(pair.first), std::move(pair.second)).first;
        }

        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        constexpr void insert(Iter first, Sentinel last) {
            sort_unique_append_<true, true>(std::ranges::subrange(first, last));
        }

        template <concepts::compatible_iterator<value_type> Iter,
                  std::sentinel_for<Iter> Sentinel>
        constexpr void insert(detail::sorted_t, Iter first, Sentinel last) {
            sort_unique_append_<true, false>(std::ranges::subrange(first, last));
        }

        constexpr void insert(std::initializer_list<value_type> il) {
            sort_unique_append_<true, true>(il);
        }

        constexpr void insert(detail::sorted_t, std::initializer_list<value_type> il) {
            sort_unique_append_<true, false>(il);
        }

        template <concepts::compatible_range<value_type> Rng>
        constexpr void insert_range(Rng&& rng) {
            sort_unique_append_<true, true>(std::forward<Rng>(rng));
        }

        template <concepts::compatible_range<value_type> Rng>
        constexpr void insert_range(detail::sorted_t, Rng&& rng) {
            sort_unique_append_<true, false>(std::forward<Rng>(rng));
        }

        [[nodiscard]] constexpr containers extract() &&
        noexcept(std::is_nothrow_move_constructible_v<containers>) {
            auto clear_anyway = detail::make_scope_guard([&]() noexcept { this->clear(); });
            auto cont = std::move(this->containers_);
            return cont;
        }

        constexpr void replace(key_container_type&& key_cont, mapped_container_type&& mapped_cont)
        noexcept(std::is_nothrow_move_assignable_v<key_container_type> &&
                 std::is_nothrow_move_assignable_v<mapped_container_type>) {
            UL_ASSERT(key_cont.size() == mapped_cont.size(), "Different sizes of key/mapped");
            UL_ASSERT(adaptor_detail::is_sorted_and_unique(key_cont.begin(), key_cont.end(), this->comp_),
                "Keys are not sorted unique");
            // try-catch block in conditionally noexcept function causes warning in some compiler
            if constexpr (std::is_nothrow_move_assignable_v<key_container_type> &&
                          std::is_nothrow_move_assignable_v<mapped_container_type>)  {
                this->keys_mut_() = std::move(key_cont);
                this->values_mut_() = std::move(mapped_cont);
            } else {
                auto clear_this_guard = detail::make_scope_guard([&]() noexcept { this->clear(); });
                this->keys_mut_() = std::move(key_cont);
                this->values_mut_() = std::move(mapped_cont);
                clear_this_guard.release();
            }
        }

        constexpr iterator erase(const_iterator cpos) {
            return this->erase_impl_(cpos);
        }

        constexpr iterator erase(iterator pos) {
            return this->erase_impl_(pos);
        }

        constexpr iterator erase(const_iterator begin, const_iterator end) {
            auto [key_begin, value_begin] = begin.key_and_mapped_iter_();
            auto [key_end, value_end] = end.key_and_mapped_iter_();
            try {
                auto key_pos_after = this->keys_mut_().erase(key_begin, key_end);
                auto value_pos_after = this->values_mut_().erase(value_begin, value_end);
                return iterator{key_pos_after, value_pos_after};
            } catch (...) {
                this->clear();
                throw;
            }
        }

        constexpr size_type erase(const key_type& key) {
            return this->erase_key_impl_(key);
        }

        template <typename Key>
        requires is_valid_key_v<Key> &&
                 (!std::convertible_to<Key, const_iterator>) &&
                 (!std::convertible_to<Key, iterator>)
        constexpr size_type erase(const Key& key) {
            return this->erase_key_impl_(key);
        }

        template <std::predicate<const_reference> Pred>
        constexpr size_type erase_if(Pred&& pred) {
            try {
                adaptor_detail::zip_view<key_container_type, mapped_container_type> zv{
                    this->keys_mut_(), this->values_mut_()
                };
                auto dup_start =
                    std::ranges::remove_if(zv.begin(), zv.end(), std::forward<Pred>(pred)).begin();
                auto offset = dup_start - zv.begin();
                auto num_erased = zv.end() - dup_start;
                this->erase(this->begin() + offset, this->end());
                return num_erased;
            } catch (...) {
                this->clear();
                throw;
            }
        }

        constexpr void clear() noexcept {
            this->containers_.key_container_.clear();
            this->containers_.mapped_container_.clear();
        }

        constexpr void swap(flat_map& other)
        noexcept(std::is_nothrow_swappable_v<key_container_type> &&
                 std::is_nothrow_swappable_v<mapped_container_type> &&
                 std::is_nothrow_swappable_v<key_compare> &&
                 std::is_nothrow_swappable_v<lower_bound_function_type>) {
            using std::swap;
            swap(this->keys_mut_(), other.keys_mut_());
            swap(this->values_mut_(), other.values_mut_());
            swap(this->comp_, other.comp_);
            swap(this->lower_bound_fn_, other.lower_bound_fn_);
        }

        //********************* Non-member functions **********************//

        friend constexpr bool operator==(const flat_map& lhs, const flat_map& rhs)
        requires std::equality_comparable<value_type> {
            return std::equal(lhs.cbegin(), lhs.cend(), rhs.begin());
        }

        friend constexpr auto operator<=>(const flat_map& lhs, const flat_map& rhs)
        requires std::three_way_comparable<key_type> &&
                 std::three_way_comparable<mapped_type> {
            auto cmp = [](const auto& a, const auto& b) {
                if (auto c = a.first <=> b.first; c != 0) {
                    return c;
                }
                return a.second <=> b.second;
            };
            return std::lexicographical_compare_three_way(lhs.begin(), lhs.end(), rhs.begin(), rhs.end(), cmp);
        }

        friend constexpr void swap(flat_map& lhs, flat_map& rhs)
        noexcept(noexcept(lhs.swap(rhs))) {
            lhs.swap(rhs);
        }
    };

    namespace adaptor_detail {
        template <typename>
        struct is_flat_map : std::false_type {};

        template <typename K, typename M, typename C, typename KC, typename MC, typename LB>
        struct is_flat_map<flat_map<K, M, C, KC, MC, LB>> : std::true_type {};
    }

    template <typename T>
    inline constexpr bool is_urlicht_flat_map_v = adaptor_detail::is_flat_map<T>::value;

    //************************* CTAD Guides **************************//

    namespace adaptor_detail {
        template <typename Iter>
        using iter_key_t = std::remove_const_t<typename std::iter_value_t<Iter>::first_type>;

        template <typename Iter>
        using iter_mapped_t = std::remove_const_t<typename std::iter_value_t<Iter>::second_type>;

        template <typename Rng>
        using rng_key_t = std::remove_const_t<typename std::ranges::range_value_t<Rng>::first_type>;

        template <typename Rng>
        using rng_mapped_t = std::remove_const_t<typename std::ranges::range_value_t<Rng>::second_type>;
    }

    template <typename KeyCont_, typename MappedCont_,
              typename KeyComp_ = std::less<>,
              typename LowerBound_ = lower_bound_fn>
    requires concepts::contiguous_container<KeyCont_> && concepts::contiguous_container<MappedCont_>
            && std::strict_weak_order<KeyComp_, typename KeyCont_::value_type, typename KeyCont_::value_type>
            && adaptor_detail::lower_bound_for<LowerBound_, typename KeyCont_::value_type, KeyCont_, KeyComp_>
    flat_map(KeyCont_, MappedCont_, KeyComp_ = KeyComp_{}, LowerBound_ = lower_bound_fn{})
        -> flat_map<typename KeyCont_::value_type,
                    typename MappedCont_::value_type,
                    KeyComp_,
                    KeyCont_,
                    MappedCont_,
                    LowerBound_>;

    template <typename KeyCont_, typename MappedCont_,
              typename KeyComp_ = std::less<>,
              typename LowerBound_ = lower_bound_fn>
    requires concepts::contiguous_container<KeyCont_> && concepts::contiguous_container<MappedCont_>
            && std::strict_weak_order<KeyComp_, typename KeyCont_::value_type, typename KeyCont_::value_type>
            && adaptor_detail::lower_bound_for<LowerBound_, typename KeyCont_::value_type, KeyCont_, KeyComp_>
    flat_map(detail::sorted_t, KeyCont_, MappedCont_, KeyComp_ = KeyComp_{}, LowerBound_ = lower_bound_fn{})
        -> flat_map<typename KeyCont_::value_type,
                    typename MappedCont_::value_type,
                    KeyComp_,
                    KeyCont_,
                    MappedCont_,
                    LowerBound_>;

    template <typename KeyCont_, typename MappedCont_,
              typename KeyComp_ = std::less<>,
              typename LowerBound_ = lower_bound_fn>
    requires concepts::contiguous_container<KeyCont_> && concepts::contiguous_container<MappedCont_>
            && std::strict_weak_order<KeyComp_, typename KeyCont_::value_type, typename KeyCont_::value_type>
            && adaptor_detail::lower_bound_for<LowerBound_, typename KeyCont_::value_type, KeyCont_, KeyComp_>
    flat_map(detail::sorted_unique_t, KeyCont_, MappedCont_, KeyComp_ = KeyComp_{}, LowerBound_ = lower_bound_fn{})
        -> flat_map<typename KeyCont_::value_type,
                    typename MappedCont_::value_type,
                    KeyComp_,
                    KeyCont_,
                    MappedCont_,
                    LowerBound_>;

    template <typename Iter, typename Sent,
              typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::forward_iterator<Iter> && std::sentinel_for<Sent, Iter>
            && std::strict_weak_order<
                KeyComp_, adaptor_detail::iter_key_t<Iter>, adaptor_detail::iter_key_t<Iter>>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, adaptor_detail::iter_key_t<Iter>,
                std::vector<adaptor_detail::iter_key_t<Iter>>, KeyComp_>
    flat_map(Iter, Sent, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<adaptor_detail::iter_key_t<Iter>,
                    adaptor_detail::iter_mapped_t<Iter>,
                    KeyComp_,
                    std::vector<adaptor_detail::iter_key_t<Iter>>,
                    std::vector<adaptor_detail::iter_mapped_t<Iter>>,
                    LowerBoundFn_>;

    template <typename Iter, typename Sent,
              typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::forward_iterator<Iter> && std::sentinel_for<Sent, Iter>
            && std::strict_weak_order<
                KeyComp_, adaptor_detail::iter_key_t<Iter>, adaptor_detail::iter_key_t<Iter>>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, adaptor_detail::iter_key_t<Iter>,
                std::vector<adaptor_detail::iter_key_t<Iter>>, KeyComp_>
    flat_map(detail::sorted_t, Iter, Sent, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<adaptor_detail::iter_key_t<Iter>,
                    adaptor_detail::iter_mapped_t<Iter>,
                    KeyComp_,
                    std::vector<adaptor_detail::iter_key_t<Iter>>,
                    std::vector<adaptor_detail::iter_mapped_t<Iter>>,
                    LowerBoundFn_>;

    template <typename Iter, typename Sent,
              typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::forward_iterator<Iter> && std::sentinel_for<Sent, Iter>
            && std::strict_weak_order<
                KeyComp_, adaptor_detail::iter_key_t<Iter>, adaptor_detail::iter_key_t<Iter>>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, adaptor_detail::iter_key_t<Iter>,
                std::vector<adaptor_detail::iter_key_t<Iter>>, KeyComp_>
    flat_map(detail::sorted_unique_t, Iter, Sent, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<adaptor_detail::iter_key_t<Iter>,
                    adaptor_detail::iter_mapped_t<Iter>,
                    KeyComp_,
                    std::vector<adaptor_detail::iter_key_t<Iter>>,
                    std::vector<adaptor_detail::iter_mapped_t<Iter>>,
                    LowerBoundFn_>;

    template <typename Rng, typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::ranges::forward_range<Rng>
            && (!is_urlicht_flat_map_v<std::remove_cvref_t<Rng>>)
            && std::strict_weak_order<
                KeyComp_, adaptor_detail::rng_key_t<Rng>, adaptor_detail::rng_key_t<Rng>>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, adaptor_detail::rng_key_t<Rng>,
                std::vector<adaptor_detail::rng_key_t<Rng>>, KeyComp_>
    flat_map(Rng, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<adaptor_detail::rng_key_t<Rng>,
                    adaptor_detail::rng_mapped_t<Rng>,
                    KeyComp_,
                    std::vector<adaptor_detail::rng_key_t<Rng>>,
                    std::vector<adaptor_detail::rng_mapped_t<Rng>>,
                    LowerBoundFn_>;

    template <typename Rng, typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::ranges::forward_range<Rng>
            && (!is_urlicht_flat_map_v<std::remove_cvref_t<Rng>>)
            && std::strict_weak_order<
                KeyComp_, adaptor_detail::rng_key_t<Rng>, adaptor_detail::rng_key_t<Rng>>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, adaptor_detail::rng_key_t<Rng>,
                std::vector<adaptor_detail::rng_key_t<Rng>>, KeyComp_>
    flat_map(detail::sorted_t, Rng, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<adaptor_detail::rng_key_t<Rng>,
                    adaptor_detail::rng_mapped_t<Rng>,
                    KeyComp_,
                    std::vector<adaptor_detail::rng_key_t<Rng>>,
                    std::vector<adaptor_detail::rng_mapped_t<Rng>>,
                    LowerBoundFn_>;

    template <typename Rng, typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::ranges::forward_range<Rng>
            && (!is_urlicht_flat_map_v<std::remove_cvref_t<Rng>>)
            && std::strict_weak_order<
                KeyComp_, adaptor_detail::rng_key_t<Rng>, adaptor_detail::rng_key_t<Rng>>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, adaptor_detail::rng_key_t<Rng>,
                std::vector<adaptor_detail::rng_key_t<Rng>>, KeyComp_>
    flat_map(detail::sorted_unique_t, Rng, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<adaptor_detail::rng_key_t<Rng>,
                    adaptor_detail::rng_mapped_t<Rng>,
                    KeyComp_,
                    std::vector<adaptor_detail::rng_key_t<Rng>>,
                    std::vector<adaptor_detail::rng_mapped_t<Rng>>,
                    LowerBoundFn_>;

    template <typename Pair,
              typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::strict_weak_order<KeyComp_, typename Pair::first_type, typename Pair::first_type>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, typename Pair::first_type,
                std::vector<typename Pair::first_type>, KeyComp_>
    flat_map(std::initializer_list<Pair>, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<typename Pair::first_type,
                    typename Pair::second_type,
                    KeyComp_,
                    std::vector<typename Pair::first_type>,
                    std::vector<typename Pair::second_type>,
                    LowerBoundFn_>;

    template <typename Pair,
              typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::strict_weak_order<KeyComp_, typename Pair::first_type, typename Pair::first_type>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, typename Pair::first_type,
                std::vector<typename Pair::first_type>, KeyComp_>
    flat_map(detail::sorted_t, std::initializer_list<Pair>, KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<typename Pair::first_type,
                    typename Pair::second_type,
                    KeyComp_,
                    std::vector<typename Pair::first_type>,
                    std::vector<typename Pair::second_type>,
                    LowerBoundFn_>;

    template <typename Pair,
              typename KeyComp_ = std::less<>,
              typename LowerBoundFn_ = lower_bound_fn>
    requires std::strict_weak_order<KeyComp_, typename Pair::first_type, typename Pair::first_type>
            && adaptor_detail::lower_bound_for<LowerBoundFn_, typename Pair::first_type,
                std::vector<typename Pair::first_type>, KeyComp_>
    flat_map(detail::sorted_unique_t, std::initializer_list<Pair>,
             KeyComp_ = KeyComp_{}, LowerBoundFn_ = LowerBoundFn_{})
        -> flat_map<typename Pair::first_type,
                    typename Pair::second_type,
                    KeyComp_,
                    std::vector<typename Pair::first_type>,
                    std::vector<typename Pair::second_type>,
                    LowerBoundFn_>;
}



#endif //URLICHT_FLAT_MAP_H
