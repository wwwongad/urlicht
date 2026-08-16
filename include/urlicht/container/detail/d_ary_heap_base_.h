#ifndef URLICHT_D_ARY_HEAP_BASE_H
#define URLICHT_D_ARY_HEAP_BASE_H

#include <urlicht/internal/config.h>
#include <urlicht/concepts/concepts.h>
#include <urlicht/internal/tag.h>
#include <urlicht/internal/scope_guard.h>
#include <urlicht/internal/allocator_of_t.h>
#include <urlicht/container/detail/is_d_ary_heapified_.h>
#include <algorithm>
#include <type_traits>
#include <utility>
#include <functional>
#include <variant>
#include <limits>
#include <optional>
#include <memory>
#include <vector>


namespace urlicht::container {

    // Policy struct for the config of urlicht::container::d_ary_heap in compile time
    struct d_ary_heap_policy {
        // Maximum number of children each node can have. Defaults to 4.
        // Guidelines:
        // Promote-dominated workload (e.g. Dijkstra on dense graph): Choose large arity (8/16/32)
        // Demote-dominated workload (e.g. k-smallest, heap-sort): Choose smaller arity (2/4)
        // Note: Do NOT use an arity that is not a power-of-two
        //
        std::size_t arity = 4;
        bool mutable_ = false;
        bool stable = false;
        bool reuse_id = true;
        bool track_generation = true;
    };

    namespace detail {
        // Actual value_type of the container
        template <bool Stable, bool Mutable, typename T, typename Counter, typename IdT>
        using heap_stored_type_ =
        std::conditional_t<
            Stable,
            std::conditional_t<Mutable, std::pair<std::pair<T, Counter>, IdT>, std::pair<T, Counter>>,
            std::conditional_t<Mutable, std::pair<T, IdT>, T>
        >;

        template <
            typename T,
            template <typename> class Container,
            typename Comp,
            d_ary_heap_policy HeapPolicy,
            typename StabilityCounter
        >
        class d_ary_heap_base_ {
        public:
            //********************** Static Methods **********************//

            [[nodiscard]] static consteval bool is_stable() noexcept {
                return HeapPolicy.stable;
            }
            [[nodiscard]] static consteval bool is_mutable() noexcept {
                return HeapPolicy.mutable_;
            }
            [[nodiscard]] static consteval std::size_t arity() noexcept {
                return HeapPolicy.arity;
            }
            [[nodiscard]] static consteval bool reuse_id() noexcept {
                return HeapPolicy.mutable_ && HeapPolicy.reuse_id;
            }
            [[nodiscard]] static consteval bool track_gen() noexcept {
                return HeapPolicy.mutable_ && HeapPolicy.reuse_id && HeapPolicy.track_generation;
            }

        protected:
            using self_type_ = d_ary_heap_base_;
            // We assert that the size_type of a container should not be dependent on value_type
            using apriori_size_type_ = typename Container<T>::size_type;
            using stored_type_ =
                detail::heap_stored_type_<
                    is_stable(), is_mutable(), T, StabilityCounter, apriori_size_type_
                >;

        public:
            using value_type = T;
            using container_type = Container<stored_type_>;
            using size_type = typename container_type::size_type;
            using difference_type = typename container_type::difference_type;

            using reference = value_type&;
            using const_reference = const value_type&;
            using pointer = value_type*;
            using const_pointer = const value_type*;
            // StabilityCounter if {stable}, else void
            using counter_type = std::conditional_t<is_stable(), StabilityCounter, void>;
            // Container<T>::size_type if {mutable}, else void
            using id_type = std::conditional_t<is_mutable(), apriori_size_type_, void>;

            using value_compare = Comp; // A comparator adaptor may be used internally

            static_assert(urlicht::concepts::object<T>, "T must satisfy std::is_object_v");
            static_assert(arity() > 1, "Arity must be greater than 1");
            static_assert(urlicht::concepts::contiguous_container<container_type>,
                          "Container must model concept contiguous_container.");
            static_assert(std::unsigned_integral<StabilityCounter>,
                          "Counter must model concept std::unsigned_integral.");
            static_assert(!is_mutable() || std::same_as<id_type, size_type>,
                          "Mismatch of Container's size_type across different value_types");

        protected:
            using container_allocator_type_ =
                std::conditional_t<
                    urlicht::concepts::has_allocator<container_type>,
                    // allocator_type_of is well-formed even if Container has no allocator_type,
                    // in which case it returns void, so the conditional_t is also well-formed
                    urlicht::internal::allocator_of_t<container_type>,
                    std::allocator<value_type>
                >;
            using maybe_counter_type_ = std::conditional_t<is_stable(), StabilityCounter, std::monostate>;
            // Maps id to position
            using position_map_type_ = std::conditional_t<is_mutable(), Container<size_type>, std::monostate>;
            using maybe_id_type_ = std::conditional_t<is_mutable(), apriori_size_type_, std::monostate>;
            // Collection of retired ids
            using free_id_pool_type_ = std::conditional_t<reuse_id(), Container<maybe_id_type_>, std::monostate>;
            using maybe_generation_type_ = std::conditional_t<track_gen(), std::uint32_t, std::monostate>;
            // Maps id to generation
            using generation_map_type_ =
            std::conditional_t<track_gen(), Container<maybe_generation_type_>, std::monostate>;

            static constexpr size_type npos_ = std::numeric_limits<size_type>::max();

            template <typename Alloc>
            static constexpr bool uses_allocator_ =
                std::uses_allocator_v<container_type, Alloc> &&
                (!is_mutable() || std::uses_allocator_v<position_map_type_, Alloc>) &&
                (!reuse_id() || std::uses_allocator_v<free_id_pool_type_, Alloc>) &&
                (!track_gen() || std::uses_allocator_v<generation_map_type_, Alloc>);

            template <typename Obj, typename Alloc, typename... Args>
            static constexpr bool must_make_obj_using_allocator_ =
                std::constructible_from<Obj, Args..., const Alloc&> ||
                std::constructible_from<Obj, std::allocator_arg_t, const Alloc&, Args...>;

            static_assert(!urlicht::concepts::has_allocator<container_type> ||
                          urlicht::concepts::allocator<container_allocator_type_>,
                          "Container<T>::allocator_type is an invalid allocator type");

            //********************* Projections *********************//

            template <typename Stored_>
            static constexpr decltype(auto) value_of_(Stored_& stored) noexcept {
                if constexpr (!is_stable() && !is_mutable()) {
                    return stored;
                } else if constexpr (is_stable() && is_mutable()) {
                    return std::get<0>(std::get<0>(stored));
                } else {
                    return std::get<0>(stored);
                }
            }

            template <typename Stored_>
            static constexpr decltype(auto) counter_of_(Stored_& stored) noexcept
            requires (is_stable()) {
                if constexpr (is_mutable()) {
                    return std::get<1>(std::get<0>(stored));
                } else {
                    return std::get<1>(stored);
                }
            }

            template <typename Stored_>
            static constexpr decltype(auto) id_of_(Stored_& stored) noexcept
            requires (is_mutable()) {
                return std::get<1>(stored);
            }

            //********************* Iterator *********************//

            // Iterator adaptor for iterating over keys instead of internal nodes.
            // This is always const to preserve invariants from external modifications.
            class iter_adaptor_ {
                using base_iterator_ = typename container_type::const_iterator;
                using self_type = iter_adaptor_;
            public:
                using iterator_category = std::random_access_iterator_tag; // Keys are separated by Counter
                using value_type = d_ary_heap_base_::value_type;
                using difference_type = d_ary_heap_base_::difference_type;
                using reference = const value_type&;
                using pointer = const value_type*;

                // Constructors
                constexpr iter_adaptor_() noexcept = default;
                constexpr explicit iter_adaptor_(base_iterator_ it) noexcept : it_(std::move(it)) {}

                constexpr iter_adaptor_(const self_type& other) noexcept = default;
                constexpr iter_adaptor_(self_type&& other) noexcept = default;
                constexpr iter_adaptor_& operator=(const self_type& other) noexcept = default;
                constexpr iter_adaptor_& operator=(self_type&& other) noexcept = default;
                constexpr ~iter_adaptor_() noexcept = default;

                constexpr reference operator*() const noexcept {
                    return value_of_(*it_);
                }

                constexpr pointer operator->() const noexcept {
                    return &value_of_(*it_);
                }

                constexpr reference operator[](difference_type idx) const noexcept {
                    return value_of_(it_[idx]);
                }

                constexpr base_iterator_ base() const noexcept {
                    return it_;
                }

                constexpr self_type& operator++() noexcept {
                    ++it_;
                    return *this;
                }

                constexpr self_type operator++(int) noexcept {
                    auto tmp = *this;
                    ++it_;
                    return tmp;
                }

                constexpr self_type& operator--() noexcept {
                    --it_;
                    return *this;
                }

                constexpr self_type operator--(int) noexcept {
                    auto tmp = *this;
                    --it_;
                    return tmp;
                }

                constexpr self_type& operator+=(difference_type n) noexcept {
                    it_ += n;
                    return *this;
                }

                constexpr self_type& operator-=(difference_type n) noexcept {
                    it_ -= n;
                    return *this;
                }

                friend constexpr self_type operator+(const self_type& it, difference_type n) noexcept {
                    return self_type(it.it_ + n);
                }

                friend constexpr self_type operator+(difference_type n, const self_type& it) noexcept {
                    return self_type(it.it_ + n);
                }

                friend constexpr self_type operator-(const self_type& it, difference_type n) noexcept {
                    return self_type(it.it_ - n);
                }

                friend constexpr difference_type operator-(const self_type& lhs, const self_type& rhs) noexcept {
                    return lhs.it_ - rhs.it_;
                }

                friend constexpr bool operator==(const self_type& lhs, const self_type& rhs) noexcept {
                    return lhs.it_ == rhs.it_;
                }

                friend constexpr auto operator<=>(const self_type& lhs, const self_type& rhs) noexcept {
                    return lhs.it_ <=> rhs.it_;
                }

            private:
                base_iterator_ it_;
            };

        public:
            using const_iterator = iter_adaptor_;
            using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        protected:
            //********************* Handle *********************//
            template <typename = void>
            class node_handle_ {
            public:
                friend class d_ary_heap_base_;

                // Default-initialization creates an invalid handle. You can use it as a placeholder.
                constexpr node_handle_() noexcept = default;
                constexpr node_handle_(const node_handle_&) noexcept = default;
                constexpr node_handle_& operator=(const node_handle_&) noexcept = default;

                [[nodiscard]] constexpr auto id() const noexcept { return node_id_; }
                [[nodiscard]] constexpr auto gen() const noexcept requires (track_gen()) { return id_gen_; }

                // If false, the handle is guaranteed to be invalid.
                [[nodiscard]] constexpr bool maybe_valid() const noexcept { return node_id_ != npos_; }
                constexpr void invalidate() noexcept { node_id_ = npos_; }
            private:
                constexpr explicit node_handle_(const maybe_id_type_ id, const maybe_generation_type_ gen) noexcept
                    : node_id_{id}, id_gen_{gen} {}

                maybe_id_type_ node_id_{npos_};
                UL_NO_UNIQUE_ADDRESS maybe_generation_type_ id_gen_{};
            };

            [[nodiscard]] constexpr auto make_handle_(const maybe_id_type_ id,
                                                     const maybe_generation_type_ gen) const noexcept {
                return node_handle_<>{id, gen};
            }

        public:
            using handle_type = std::conditional_t<is_mutable(), node_handle_<>, std::monostate>;

        protected:
            //********************* Comparator *********************//
            static_assert(std::strict_weak_order<Comp, const_reference, const_reference>,
                          "Comp must be a valid comparator for value_type");

            // Comparator adaptor with correct handling of ties (if applicable)
            // We always use this for to compare stored_type_
            class comparator_adaptor_ {
            public:
                constexpr comparator_adaptor_()
                    noexcept(std::is_nothrow_default_constructible_v<value_compare>)
                = default;

                template <urlicht::concepts::can_construct<value_compare> ValueComp>
                constexpr comparator_adaptor_(ValueComp comp)
                    noexcept(std::is_nothrow_move_constructible_v<value_compare>)
                : comp_{std::move(comp)} {  }

                constexpr comparator_adaptor_(const comparator_adaptor_& other) = default;

                constexpr comparator_adaptor_(comparator_adaptor_&& other)
                    noexcept(std::is_nothrow_move_constructible_v<value_compare>)
                = default;

                constexpr comparator_adaptor_& operator=(const comparator_adaptor_& other) = default;

                constexpr comparator_adaptor_& operator=(comparator_adaptor_&& other)
                    noexcept(std::is_nothrow_move_assignable_v<value_compare>)
                = default;

                constexpr ~comparator_adaptor_() noexcept = default;

                // stored_comp_(a, b) == true iff a has lower priority than b
                // std::less<> yields a max-heap, std::greater<> yields a min-heap
                constexpr bool operator()(const stored_type_& lhs, const stored_type_& rhs) const
                    noexcept(std::is_nothrow_invocable_v<value_compare, const_reference, const_reference>) {
                    if constexpr (is_stable()) {
                        return std::invoke(comp_, value_of_(lhs), value_of_(rhs)) ||
                               (!std::invoke(comp_, value_of_(rhs), value_of_(lhs)) && counter_of_(lhs) > counter_of_(rhs));
                    } else {
                        return std::invoke(comp_, value_of_(lhs), value_of_(rhs));
                    }
                }

                constexpr const auto& base() const noexcept { return comp_; }

                constexpr void swap(comparator_adaptor_& other)
                    noexcept(std::is_nothrow_swappable_v<value_compare>) {
                    using std::swap;
                    swap(comp_, other.comp_);
                }

                friend void swap(comparator_adaptor_& lhs, comparator_adaptor_& rhs)
                    noexcept(std::is_nothrow_swappable_v<value_compare>) {
                    lhs.swap(rhs);
                }
            private:
                UL_NO_UNIQUE_ADDRESS value_compare comp_;
            };

            using stored_compare_ = comparator_adaptor_;

            //************************** Core Kernel methods **************************//

            [[nodiscard]] constexpr maybe_id_type_ acquire_id_()
            requires (is_mutable()) {
                if constexpr (reuse_id()) {
                    if (!free_id_pool_.empty()) {
                        const auto free_id = free_id_pool_.back();
                        free_id_pool_.pop_back();
                        return free_id;
                    }
                }
                const id_type new_id = id_to_pos_map_.size();
                id_to_pos_map_.emplace_back(npos_);

                if constexpr (track_gen()) {
                    id_to_gen_map_.emplace_back();
                }
                return new_id;
            }

            constexpr void free_id_(const maybe_id_type_ id)
            requires (is_mutable()) {
                id_to_pos_map_[id] = npos_;

                if constexpr (reuse_id()) {
                    free_id_pool_.emplace_back(id);
                }
                if constexpr (track_gen()) {
                    ++id_to_gen_map_[id];
                }
            }

            constexpr maybe_generation_type_ gen_of_id_(maybe_id_type_ id) const noexcept
            requires (is_mutable()) {
                if constexpr (track_gen()) {
                    return id_to_gen_map_[id];
                } else {
                    return {};
                }
            }

            template <typename ...Args>
            constexpr void make_and_append_immutable_(Args&& ...args)  // Strong exception safety
            requires (!is_mutable()) {
                if constexpr (is_stable()) {
                    container_.emplace_back(
                        std::piecewise_construct,
                        std::forward_as_tuple(std::forward<Args>(args)...),
                        std::forward_as_tuple(this->counter_++));
                } else {
                    container_.emplace_back(std::forward<Args>(args)...);
                }
            }

            // Not exception-safe
            template <typename ...Args>
            constexpr handle_type make_and_append_mutable_(Args&& ...args)
            requires (is_mutable()) {
                auto id = acquire_id_();
                const auto pos = container_.size();

                if constexpr (is_stable()) {
                    container_.emplace_back(
                        std::piecewise_construct,
                        std::forward_as_tuple(
                            std::piecewise_construct,
                            std::forward_as_tuple(std::forward<Args>(args)...),
                            std::forward_as_tuple(this->counter_++)),
                        std::forward_as_tuple(id));
                } else {
                    container_.emplace_back(
                        std::piecewise_construct,
                        std::forward_as_tuple(std::forward<Args>(args)...),
                        std::forward_as_tuple(id));
                }

                id_to_pos_map_[id] = pos;
                return make_handle_(id, gen_of_id_(id));
            }

            template <typename Rng>
            constexpr void maybe_reserve_for_([[maybe_unused]] const Rng& rng,
                                              [[maybe_unused]] size_type& append_size) {
                if constexpr (std::ranges::sized_range<Rng>) {
                    const auto n = this->container_.size();
                    append_size = std::ranges::size(rng);
                    this->container_.reserve(n + append_size);
                }
            }

            // Exception handling is left to the call site
            template <typename Rng_>
            constexpr size_type container_append_range_(Rng_&& rng) {
                [[maybe_unused]] size_type append_size{0U};
                maybe_reserve_for_(rng, append_size);

                for (auto&& value : rng) {
                    if constexpr (!is_mutable()) {
                        make_and_append_immutable_(std::forward<decltype(value)>(value));
                    } else {
                        make_and_append_mutable_(std::forward<decltype(value)>(value));
                    }
                    if constexpr (!std::ranges::sized_range<Rng_>) {
                        ++append_size;
                    }
                }
                return append_size;
            }

            template <typename Rng_, typename OutputIt>
            constexpr size_type container_append_range_(Rng_&& rng, OutputIt o_it) {
                [[maybe_unused]] size_type append_size{0U};
                maybe_reserve_for_(rng, append_size);

                for (auto&& value : rng) {
                    *o_it++ = make_and_append_mutable_(std::forward<decltype(value)>(value));
                    if constexpr (!std::ranges::sized_range<Rng_>) {
                        ++append_size;
                    }
                }
                return append_size;
            }

            constexpr void rebuild_or_heapify_up_(const size_type old_size,
                                                  const size_type append_size, bool rebuild_hint) {
                // Heuristics slightly in favor of rebuilding the heap
                if (rebuild_hint || append_size > old_size / arity()) {
                    build_heap_();
                } else {
                    auto new_size = old_size + append_size;
                    for (size_type pos = old_size; pos < new_size; ++pos) {
                        heapify_up_(pos);
                    }
                }
            }

            template <typename Stored_>
            constexpr void relocate_to_(Stored_&& stored_elem, const size_type dst_idx)
                noexcept(std::is_nothrow_move_assignable_v<Stored_>){
                container_[dst_idx] = std::forward<Stored_>(stored_elem);
                if constexpr (is_mutable()) {
                    id_to_pos_map_[id_of_(container_[dst_idx])] = dst_idx;
                }
            }

            template <bool IsFullTree = false>
            constexpr void heapify_down_(size_type idx, const size_type sift_down_size) {
                stored_type_ top_elem = std::move(container_[idx]);
                while (true) {
                    auto child_idx = static_cast<size_type>(idx * arity() + 2);  // Second children (Arity > 1)
                    auto largest_child = child_idx - 1;

                    // Check the current node is not a leaf
                    if (largest_child >= sift_down_size) [[unlikely]] {
                        break;
                    }
                    size_type last_child_idx;
                    if constexpr (IsFullTree) {
                        last_child_idx = idx * arity() + arity();
                    } else {
                        last_child_idx = std::min(idx * arity() + arity(), sift_down_size - 1);
                    }
                    while (child_idx <= last_child_idx) {
                        /*
                        if (std::invoke(comp_, container_[largest_child], container_[child_idx])) {
                            largest_child = child_idx;
                        }
                        */
                        // Branchless version:
                        largest_child +=
                            (child_idx - largest_child) &
                            -static_cast<size_type>(
                                std::invoke(comp_, container_[largest_child], container_[child_idx])
                            );
                        ++child_idx;
                    }
                    if (!std::invoke(comp_, top_elem, container_[largest_child])) {
                        break;
                    }
                    relocate_to_(std::move(container_[largest_child]), idx);
                    idx = largest_child;
                }
                relocate_to_(std::move(top_elem), idx);
            }

            constexpr void heapify_down_(size_type idx) {
                heapify_down_<false>(idx, container_.size());
            }

            constexpr void build_heap_() {
                // Optimization from LLVM:
                // If stored_type_ is cheap to move (based on which we also assume that comparison is cheap),
                // it may be beneficial to remove the out-of-bound checks in heapify_down_.
                constexpr bool to_truncate = std::is_trivially_move_assignable_v<stored_type_>;
                const auto n = container_.size();

                if (n > 1) [[likely]] {
                    size_type sift_down_size = n;
                    if constexpr (to_truncate) {
                        sift_down_size = n - (n - 1) % arity();
                    }

                    if (sift_down_size > 1) [[likely]] {
                        for (size_type start = (sift_down_size - 2) / arity() + 1; start > 0; --start) {
                            heapify_down_<to_truncate>(start - 1, sift_down_size);
                        }
                    }
                    if constexpr (to_truncate) {
                        const size_type extra = n - sift_down_size;
                        UL_ASSUME(extra < arity());
                        for (size_type i = 0; i < extra; ++i) {
                            heapify_up_(sift_down_size + i);
                        }
                    }
                }
            }

            // The caller must ensure that pos > 0
            constexpr void heapify_up_(size_type pos) {
                UL_ASSERT(pos > 0 && pos < container_.size(), "pos out-of-range");

                auto parent = static_cast<size_type>((pos - 1) / arity());
                stored_type_ tmp = std::move(container_[pos]);

                while (pos > 0 && std::invoke(comp_, container_[parent], tmp)) {
                    relocate_to_(std::move(container_[parent]), pos);
                    pos = parent;
                    // If pos == 0 here, parent will overflow. However, it is no longer needed anyway
                    // since the while loop will break.
                    parent = (pos - 1) / arity();
                }
                relocate_to_(std::move(tmp), pos);
            }

            constexpr void heapify_up_or_down_at_(const size_type pos) {
                if (pos > 0 && std::invoke(comp_, container_[(pos - 1) / arity()], container_[pos])) {
                    heapify_up_(pos);
                } else {
                    heapify_down_(pos);
                }
            }

            enum class modify_t_ {
                unknown,
                promote,
                demote
            };

            template <modify_t_ MT = modify_t_::unknown, typename F>
            constexpr void unchecked_modify_at_impl_(const size_type idx, F&& fn) {
                UL_ASSERT(idx < container_.size(), "idx out-of-range.");
                auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });

                std::invoke(std::forward<F>(fn), value_of_(container_[idx]));
                if constexpr (MT == modify_t_::promote) {
                    if (idx > 0) [[likely]] heapify_up_(idx);
                } else if constexpr (MT == modify_t_::demote) {
                    heapify_down_(idx);
                } else {
                    heapify_up_or_down_at_(idx);
                }

                clear_guard.release();
            }

            template <typename Rng>
            constexpr void push_range_impl_(Rng&& rng, bool rebuild_hint) {
                auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
                const auto old_size = size();
                const auto append_size = container_append_range_(std::forward<Rng>(rng));
                rebuild_or_heapify_up_(old_size, append_size, rebuild_hint);
                clear_guard.release();
            }

        public:
            //********************** Constructors **********************//

            constexpr d_ary_heap_base_()
            noexcept(std::is_nothrow_default_constructible_v<stored_compare_> &&
                     std::is_nothrow_default_constructible_v<container_type>)
            requires std::default_initializable<container_type>
            = default;

            // Construct from alloc
            template <urlicht::concepts::allocator Alloc = container_allocator_type_>
            requires uses_allocator_<Alloc> &&
                    (!std::same_as<std::remove_cvref_t<Alloc>, self_type_>)
            explicit constexpr d_ary_heap_base_(const Alloc& alloc)
            : container_{std::make_obj_using_allocator<container_type>(alloc)},
              id_to_pos_map_{std::make_obj_using_allocator<position_map_type_>(alloc)},
              free_id_pool_{std::make_obj_using_allocator<free_id_pool_type_>(alloc)},
              id_to_gen_map_{std::make_obj_using_allocator<generation_map_type_>(alloc)}
            { }

            // Construct from ValueComp
            template <urlicht::concepts::can_construct<value_compare> ValueComp>
            explicit constexpr d_ary_heap_base_(ValueComp comp)
            noexcept(std::is_nothrow_default_constructible_v<container_type> &&
                     std::is_nothrow_constructible_v<stored_compare_, ValueComp&&>)
            : comp_{std::move(comp)} {  }

            // Construct from ValueComp and alloc
            template <urlicht::concepts::allocator Alloc, urlicht::concepts::can_construct<value_compare> ValueComp>
            requires uses_allocator_<Alloc>
            explicit constexpr d_ary_heap_base_(ValueComp comp, const Alloc& alloc)
            : container_{std::make_obj_using_allocator<container_type>(alloc)},
              comp_{std::move(comp)},
              id_to_pos_map_{std::make_obj_using_allocator<position_map_type_>(alloc)},
              free_id_pool_{std::make_obj_using_allocator<free_id_pool_type_>(alloc)},
              id_to_gen_map_{std::make_obj_using_allocator<generation_map_type_>(alloc)}
            { }

            // Enables direct construction of container_ from rng, which is likely more efficient.
            // This is possible for immutable-unstable heap only, where value_type == stored_type_.
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp = value_compare>
            requires (!is_mutable()) && (!is_stable()) && std::constructible_from<container_type, Rng&&>
            constexpr d_ary_heap_base_(Rng&& rng, ValueComp comp = ValueComp{})
            : container_{std::forward<Rng>(rng)},
              comp_{std::move(comp)} {
                this->build_heap_();
            }

            // Direct construction of container_ from rng and alloc
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && (!is_mutable()) && (!is_stable()) &&
                     must_make_obj_using_allocator_<container_type, Alloc, Rng&&>
            constexpr d_ary_heap_base_(Rng&& rng, const Alloc& alloc)
            : container_{std::make_obj_using_allocator<container_type>(alloc, std::forward<Rng>(rng))},
              id_to_pos_map_{std::make_obj_using_allocator<position_map_type_>(alloc)},
              free_id_pool_{std::make_obj_using_allocator<free_id_pool_type_>(alloc)},
              id_to_gen_map_{std::make_obj_using_allocator<generation_map_type_>(alloc)} {
                this->build_heap_();
            }

            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp,
                      urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && (!is_mutable()) && (!is_stable()) &&
                     must_make_obj_using_allocator_<container_type, Alloc, Rng&&>
            constexpr d_ary_heap_base_(Rng&& rng, ValueComp comp, const Alloc& alloc)
            : container_{std::make_obj_using_allocator<container_type>(alloc, std::forward<Rng>(rng))},
              comp_{std::move(comp)},
              id_to_pos_map_{std::make_obj_using_allocator<position_map_type_>(alloc)},
              free_id_pool_{std::make_obj_using_allocator<free_id_pool_type_>(alloc)},
              id_to_gen_map_{std::make_obj_using_allocator<generation_map_type_>(alloc)} {
                this->build_heap_();
            }

            // Indirect construction from rng
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp = value_compare>
            requires std::ranges::forward_range<Rng> &&
                     (is_mutable() || is_stable() || (!std::constructible_from<container_type, Rng&&>))
            explicit constexpr d_ary_heap_base_(Rng&& rng, ValueComp comp = ValueComp{})
            : comp_{std::move(comp)} {
                container_append_range_(std::forward<Rng>(rng));
                this->build_heap_();
            }

            // Indirect construction from rng and alloc
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && std::ranges::forward_range<Rng> &&
                    (is_mutable() || is_stable() ||
                     (!must_make_obj_using_allocator_<container_type, Alloc, Rng&&>))
            constexpr d_ary_heap_base_(Rng&& rng, const Alloc& alloc)
            : d_ary_heap_base_(alloc) {
                container_append_range_(std::forward<Rng>(rng));
                this->build_heap_();
            }

            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp,
                      urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && std::ranges::forward_range<Rng> &&
                     (is_mutable() || is_stable() ||
                     (!must_make_obj_using_allocator_<container_type, Alloc, Rng&&>))
            constexpr d_ary_heap_base_(Rng&& rng, ValueComp comp, const Alloc& alloc)
            : d_ary_heap_base_(comp, alloc) {
                container_append_range_(std::forward<Rng>(rng));
                this->build_heap_();
            }

            // Direct construction from heapified rng
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp = value_compare>
            requires (!is_mutable()) && (!is_stable()) && std::constructible_from<container_type, Rng&&>
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, Rng&& rng, ValueComp comp = ValueComp{})
            : container_{std::forward<Rng>(rng)},
              comp_{std::move(comp)} {
                UL_ASSERT(is_d_ary_heapified_<arity()>(container_, comp_), "The given range is not heapified.");
            }

            // Direct construction from heapified rng and alloc
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && (!is_mutable()) && (!is_stable()) &&
                     must_make_obj_using_allocator_<container_type, Alloc, Rng&&>
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, Rng&& rng,
                                       const Alloc& alloc)
            : container_{std::make_obj_using_allocator<container_type>(alloc, std::forward<Rng>(rng))},
              id_to_pos_map_{std::make_obj_using_allocator<position_map_type_>(alloc)},
              free_id_pool_{std::make_obj_using_allocator<free_id_pool_type_>(alloc)},
              id_to_gen_map_{std::make_obj_using_allocator<generation_map_type_>(alloc)} {
                UL_ASSERT(is_d_ary_heapified_<arity()>(container_, comp_), "The given range is not heapified.");
            }

            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp,
                      urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && (!is_mutable()) && (!is_stable()) &&
                     must_make_obj_using_allocator_<container_type, Alloc, Rng&&>
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, Rng&& rng,
                                       ValueComp comp, const Alloc& alloc)
            : container_{std::make_obj_using_allocator<container_type>(alloc, std::forward<Rng>(rng))},
              comp_{std::move(comp)},
              id_to_pos_map_{std::make_obj_using_allocator<position_map_type_>(alloc)},
              free_id_pool_{std::make_obj_using_allocator<free_id_pool_type_>(alloc)},
              id_to_gen_map_{std::make_obj_using_allocator<generation_map_type_>(alloc)} {
                UL_ASSERT(is_d_ary_heapified_<arity()>(container_, comp_), "The given range is not heapified.");
            }

            // Indirect construction from heapified rng
            template <urlicht::concepts::compatible_range<value_type> Rng,
                      urlicht::concepts::can_construct<value_compare> ValueComp = value_compare>
            requires (is_mutable() || is_stable() || (!std::constructible_from<container_type, Rng&&>))
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, Rng&& rng, ValueComp comp = ValueComp{})
            : comp_{std::move(comp)} {
                UL_ASSERT(is_d_ary_heapified_<arity()>(rng, value_comp()), "The given range is not heapified.");
                container_append_range_(std::forward<Rng>(rng));
            }

            // Indirect construction from heapified rng and alloc
            template <urlicht::concepts::compatible_range<value_type> Rng,
                     urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && (is_mutable() || is_stable() ||
                   (!must_make_obj_using_allocator_<container_type, Alloc, Rng&&>))
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, Rng&& rng,
                                      const Alloc& alloc)
            : d_ary_heap_base_(alloc) {
                UL_ASSERT(is_d_ary_heapified_<arity()>(rng, value_comp()), "The given range is not heapified.");
                container_append_range_(std::forward<Rng>(rng));
            }

            template <urlicht::concepts::compatible_range<value_type> Rng,
                     urlicht::concepts::can_construct<value_compare> ValueComp,
                     urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc> && (is_mutable() || is_stable() ||
                   (!must_make_obj_using_allocator_<container_type, Alloc, Rng&&>))
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, Rng&& rng,
                                      ValueComp comp, const Alloc& alloc)
            : d_ary_heap_base_(comp, alloc) {
                UL_ASSERT(is_d_ary_heapified_<arity()>(rng, value_comp()), "The given range is not heapified.");
                container_append_range_(std::forward<Rng>(rng));
            }

            // [Indirect] Construction from initializer_list
            template <urlicht::concepts::can_construct<value_compare> ValueComp = value_compare>
            constexpr d_ary_heap_base_(std::initializer_list<value_type> il, ValueComp comp = ValueComp{})
            : comp_{std::move(comp)} {
                container_append_range_(il);
                this->build_heap_();
            }

            // [Indirect] Construction from initializer_list and alloc
            template <urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc>
            constexpr d_ary_heap_base_(std::initializer_list<value_type> il, const Alloc& alloc)
            : d_ary_heap_base_(alloc) {
                container_append_range_(il);
                this->build_heap_();
            }

            template <urlicht::concepts::can_construct<value_compare> ValueComp, urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc>
            constexpr d_ary_heap_base_(std::initializer_list<value_type> il, ValueComp comp, const Alloc& alloc)
            : d_ary_heap_base_(comp, alloc) {
                container_append_range_(il);
                this->build_heap_();
            }

            // [Indirect] Construction from heapified initializer list
            template <urlicht::concepts::can_construct<value_compare> ValueComp = value_compare>
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, std::initializer_list<value_type> il,
                                       ValueComp comp = ValueComp{})
            : comp_{std::move(comp)} {
                UL_ASSERT(is_d_ary_heapified_<arity()>(il, value_comp()), "The given range is not heapified.");
                container_append_range_(il);
            }

            // [Indirect] Construction from heapified initializer list and alloc
            template <urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc>
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, std::initializer_list<value_type> il,
                                       const Alloc& alloc)
            : d_ary_heap_base_(alloc) {
                UL_ASSERT(is_d_ary_heapified_<arity()>(il, value_comp()), "The given range is not heapified.");
                container_append_range_(il);
            }

            template <urlicht::concepts::can_construct<value_compare> ValueComp, urlicht::concepts::allocator Alloc>
            requires uses_allocator_<Alloc>
            constexpr d_ary_heap_base_(urlicht::internal::heapified_t, std::initializer_list<value_type> il,
                                       ValueComp comp, const Alloc& alloc)
            : d_ary_heap_base_(comp, alloc) {
                UL_ASSERT(is_d_ary_heapified_<arity()>(il, value_comp()), "The given range is not heapified.");
                container_append_range_(il);
            }

            constexpr d_ary_heap_base_(const d_ary_heap_base_&) = default;

            constexpr d_ary_heap_base_(d_ary_heap_base_&& other)
            noexcept(
                std::is_nothrow_move_constructible_v<container_type> &&
                std::is_nothrow_move_constructible_v<stored_compare_> &&
                std::is_nothrow_move_constructible_v<maybe_counter_type_> &&
                std::is_nothrow_move_constructible_v<position_map_type_> &&
                std::is_nothrow_move_constructible_v<free_id_pool_type_> &&
                std::is_nothrow_move_constructible_v<generation_map_type_>
            )
            try : container_{std::move(other.container_)},
                  comp_{std::move(other.comp_)},
                  counter_{std::move(other.counter_)},
                  id_to_pos_map_{std::move(other.id_to_pos_map_)},
                  free_id_pool_{std::move(other.free_id_pool_)},
                  id_to_gen_map_{std::move(other.id_to_gen_map_)} {
                other.clear();
            } catch (...) {
                other.clear();
                if constexpr (!std::is_nothrow_move_constructible_v<d_ary_heap_base_>) {  // For gcc
                    throw;
                }
            }

            constexpr d_ary_heap_base_& operator=(const d_ary_heap_base_& other) {
                if (this == &other) [[unlikely]] {
                    return *this;
                }
                try {
                    this->container_ = other.container_;
                    this->comp_ = other.comp_;
                    this->counter_ = other.counter_;
                    this->id_to_pos_map_ = other.id_to_pos_map_;
                    this->free_id_pool_ = other.free_id_pool_;
                    this->id_to_gen_map_ = other.id_to_gen_map_;
                } catch (...) {
                    this->clear();
                    throw;
                }
                return *this;
            }

            constexpr d_ary_heap_base_& operator=(d_ary_heap_base_&& other)
            noexcept(
                std::is_nothrow_move_assignable_v<container_type> &&
                std::is_nothrow_move_assignable_v<stored_compare_> &&
                std::is_nothrow_move_assignable_v<maybe_counter_type_> &&
                std::is_nothrow_move_assignable_v<position_map_type_> &&
                std::is_nothrow_move_assignable_v<free_id_pool_type_> &&
                std::is_nothrow_move_assignable_v<generation_map_type_>
            ) {
                if (this == &other) [[unlikely]] {
                    return *this;
                }
                // Avoid using try-catch block in a conditionally noexcept method
                auto clear_other_guard = urlicht::internal::make_scope_guard([&] { other.clear(); });
                auto do_move = [this, &other] {
                    this->container_ = std::move(other.container_);
                    this->comp_ = std::move(other.comp_);
                    this->counter_ = std::move(other.counter_);
                    this->id_to_pos_map_ = std::move(other.id_to_pos_map_);
                    this->free_id_pool_ = std::move(other.free_id_pool_);
                    this->id_to_gen_map_ = std::move(other.id_to_gen_map_);
                };
                if constexpr (std::is_nothrow_move_assignable_v<self_type_>) {
                    do_move();
                } else {
                    auto clear_this_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
                    do_move();
                    clear_this_guard.release();
                }
                return *this;
            }

            constexpr ~d_ary_heap_base_() = default;

            //************************* Core Methods *************************//

            template <urlicht::concepts::compatible_range<value_type> Rng>
            constexpr void push_range(Rng&& rng, bool rebuild_hint = false) {
                push_range_impl_(std::forward<Rng>(rng), rebuild_hint);
            }

            constexpr void push_range(std::initializer_list<value_type> il, bool rebuild_hint = false) {
                push_range_impl_(il, rebuild_hint);
            }

            // As per C++26 STL convention, we provide three overloads (unchecked/try/{checked})
            // for member functions with preconditions.

            constexpr void unchecked_pop() {
                UL_ASSERT(!empty(), "The heap is empty");
                auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
                if constexpr (is_mutable()) {
                    auto top_id = id_of_(container_.front());
                    free_id_(top_id);
                }
                if (size() > 1) [[likely]] {
                    relocate_to_(std::move(container_.back()), 0U);
                    container_.pop_back();
                    heapify_down_(0U);
                } else {
                    container_.pop_back();
                }
                clear_guard.release();
            }

            constexpr bool try_pop() {
                if (empty()) [[unlikely]] {
                    return false;
                }
                unchecked_pop();
                return true;
            }

            constexpr void pop() {
                if (!try_pop()) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::pop(): the heap is empty"};
                }
            }

            template <typename VT>
            requires std::assignable_from<reference, VT&&>
            constexpr void unchecked_replace_top(VT&& val) {
                UL_ASSERT(!empty(), "The heap is empty");
                value_of_(container_.front()) = std::forward<VT>(val);
                heapify_down_(0U);
            }

            template <typename VT>
            requires std::assignable_from<reference, VT&&>
            constexpr bool try_replace_top(VT&& val) {
                if (empty()) [[unlikely]] {
                    return false;
                }
                unchecked_replace_top(std::forward<VT>(val));
                return true;
            }

            template <typename VT>
            requires std::assignable_from<reference, VT&&>
            constexpr void replace_top(VT&& val) {
                if (!try_replace_top(std::forward<VT>(val))) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::replace_top(): The heap is empty"};
                }
            }

            template <std::invocable<reference> F>
            constexpr void unchecked_modify_top(F&& func) {
                UL_ASSERT(!empty(), "The heap is empty");
                unchecked_modify_at_impl_<modify_t_::demote>(0U, std::forward<F>(func));
            }

            template <std::invocable<reference> F>
            constexpr bool try_modify_top(F&& func) {
                if (empty()) [[unlikely]] {
                    return false;
                }
                unchecked_modify_top(std::forward<F>(func));
                return true;
            }

            template <std::invocable<reference> F>
            constexpr void modify_top(F&& func) {
                if (!try_modify_top(std::forward<F>(func))) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::modify_top(): The heap is empty"};
                }
            }

            // modify_at: a way to modify values in immutable heaps, although it probably requires a O(n)
            // traversal on the user side to find the right index, making it impractical in many cases.

            template <std::invocable<reference> F>
            constexpr void unchecked_modify_at(const size_type idx, F&& fn) {
                unchecked_modify_at_impl_(idx, std::forward<F>(fn));
            }

            template <std::invocable<reference> F>
            constexpr bool try_modify_at(const size_type idx, F&& fn) {
                if (idx >= container_.size()) [[unlikely]] {
                    return false;
                }
                unchecked_modify_at(idx, std::forward<F>(fn));
                return true;
            }

            template <std::invocable<reference> F>
            constexpr void modify_at(const size_type idx, F&& fn) {
                if (!try_modify_at(idx, std::forward<F>(fn))) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::modify(): idx out-of-range"};
                }
            }

            template <std::invocable<reference> F>
            constexpr void unchecked_promote_at(const size_type idx, F&& fn) {
                UL_ASSERT(idx < size(), "Index out-of-range");
                unchecked_modify_at_impl_<modify_t_::promote>(idx, std::forward<F>(fn));
            }

            template <std::invocable<reference> F>
            constexpr bool try_promote_at(const size_type idx, F&& fn) {
                if (idx >= container_.size()) [[unlikely]] {
                    return false;
                }
                unchecked_promote_at(idx, std::forward<F>(fn));
                return true;
            }

            template <std::invocable<reference> F>
            constexpr void promote_at(const size_type idx, F&& fn) {
                if (!try_promote_at(idx, std::forward<F>(fn))) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::promote_at: index out-of-range"};
                }
            }

            template <std::invocable<reference> F>
            constexpr void unchecked_demote_at(const size_type idx, F&& fn) {
                UL_ASSERT(idx < size(), "Index out-of-range");
                unchecked_modify_at_impl_<modify_t_::demote>(idx, std::forward<F>(fn));
            }

            template <std::invocable<reference> F>
            constexpr bool try_demote_at(const size_type idx, F&& fn) {
                if (idx >= container_.size()) [[unlikely]] {
                    return false;
                }
                unchecked_demote_at(idx, std::forward<F>(fn));
                return true;
            }

            template <std::invocable<reference> F>
            constexpr void demote_at(const size_type idx, F&& fn) {
                if (!try_demote_at(idx, std::forward<F>(fn))) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::demote_at: index out-of-range"};
                }
            }

            constexpr void unchecked_erase_at(const size_type idx) {
                UL_ASSERT(idx < size(), "idx out-of-range");
                auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
                if constexpr (is_mutable()) {
                    const auto id = id_of_(container_[idx]);
                    free_id_(id);
                }
                if (idx < size() - 1) [[likely]] {
                    relocate_to_(std::move(container_.back()), idx);
                    container_.pop_back();
                    heapify_up_or_down_at_(idx);
                } else {
                    container_.pop_back();
                }
                clear_guard.release();
            }

            constexpr bool try_erase_at(const size_type idx) {
                if (idx >= container_.size()) [[unlikely]] {
                    return false;
                }
                unchecked_erase_at(idx);
                return true;
            }

            constexpr void erase_at(const size_type idx) {
                if (!try_erase_at(idx)) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::erase_at(): position out-of-range"};
                }
            }

            [[nodiscard]] constexpr value_type unchecked_extract_at(const size_type idx) {
                UL_ASSERT(idx < size(), "idx out-of-range");
                value_type value = std::move(value_of_(container_[idx]));
                unchecked_erase_at(idx);  // Erase the moved-from element
                return value;
            }

            [[nodiscard]] constexpr std::optional<value_type> try_extract_at(const size_type idx) {
                if (idx >= size()) [[unlikely]] {
                    return std::nullopt;
                }
                auto opt = std::make_optional(std::move(value_of_(container_[idx])));
                unchecked_erase_at(idx);  // Erase the moved-from element
                return opt;
            }

            [[nodiscard]] constexpr value_type extract_at(const size_type idx) {
                if (idx >= size()) [[unlikely]] {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::extract_at(): idx out-of-range"};
                }
                return unchecked_extract_at(idx);
            }

            // Extract all elements to the given output iterator in current heapified order
            template <std::output_iterator<value_type> OutputIt>
            constexpr void extract_all(OutputIt o_it) {
                auto n = container_.size();
                for (size_type i = 0U; i < n; ++i) {
                    *o_it++ = std::move(value_of_(container_[i]));
                }
                clear();
            }

            // Extract all elements to the given output iterator in a sorted order
            template <std::output_iterator<value_type> OutputIt>
            constexpr void extract_sorted(OutputIt o_it) {
                auto back_idx = container_.size() - 1;
                while (back_idx >= 1) {
                    *o_it++ = std::move(value_of_(container_.front()));
                    relocate_to_(std::move(container_[back_idx]), 0U);
                    --back_idx;
                    heapify_down_(0U, back_idx + 1);
                }
                *o_it++ = std::move(value_of_(container_.front()));
                container_.clear();
            }

            //************************* Iterators *************************//
            // Const iterators only.

            [[nodiscard]] constexpr const_iterator cbegin() const noexcept {
                return const_iterator{container_.cbegin()};
            }
            [[nodiscard]] constexpr const_iterator cend() const noexcept {
                return const_iterator{container_.cend()};
            }
            [[nodiscard]] constexpr const_iterator begin() const noexcept {
                return cbegin();
            }
            [[nodiscard]] constexpr const_iterator end() const noexcept {
                return cend();
            }

            [[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept {
                return const_reverse_iterator{cend()};
            }
            [[nodiscard]] constexpr const_reverse_iterator crend() const noexcept {
                return const_reverse_iterator{cbegin()};
            }
            [[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept {
                return crbegin();
            }
            [[nodiscard]] constexpr const_reverse_iterator rend() const noexcept {
                return crend();
            }

            [[nodiscard]] constexpr difference_type iter_index_of(const_iterator cit) const noexcept {
                return cit - cbegin();
            }
            [[nodiscard]] constexpr const_iterator nth(const size_type idx) const noexcept {
                UL_ASSERT(idx < container_.size(), "idx out-of-range.");
                return const_iterator{container_.cbegin() + idx};
            }

            //************************* Observers *************************//

            [[nodiscard]] constexpr const_reference unchecked_top() const noexcept {
                UL_ASSERT(!empty(), "The heap is empty");
                return value_of_(container().front());
            }

            [[nodiscard]] constexpr const_reference top() const {
                if (empty()) {
                    throw std::out_of_range{"urlicht::container::d_ary_heap::top(): The heap is empty."};
                }
                return unchecked_top();
            }

            // Returns the [raw] value comparator
            [[nodiscard]] constexpr const value_compare& value_comp() const noexcept {
                return comp_.base();
            }

            [[nodiscard]] constexpr const container_type& container() const noexcept {
                return container_;
            }

            [[nodiscard]] constexpr const_reference operator[](const size_type idx) const noexcept {
                UL_ASSERT(idx < size(), "Index out-of-range");
                return value_of_(container_[idx]);
            }

            //************************* Capacity *************************//

            [[nodiscard]] constexpr size_type max_size() const noexcept {
                return container_.max_size();
            }

            [[nodiscard]] constexpr size_type size() const noexcept {
                return container_.size();
            }

            [[nodiscard]] constexpr bool empty() const noexcept {
                return container_.empty();
            }

            constexpr void reserve(const size_type n) {
                // contiguous container must be reservable
                container_.reserve(n);
                if constexpr (is_mutable()) id_to_pos_map_.reserve(n);
                if constexpr (reuse_id()) free_id_pool_.reserve(n);
                if constexpr (track_gen()) id_to_gen_map_.reserve(n);
            }

            constexpr void shrink_to_fit() {
                if constexpr (urlicht::concepts::reservable_container<container_type>) {
                    container_.shrink_to_fit();
                    if constexpr (is_mutable()) id_to_pos_map_.shrink_to_fit();
                    if constexpr (reuse_id()) free_id_pool_.shrink_to_fit();
                    if constexpr (track_gen()) id_to_gen_map_.shrink_to_fit();
                }
            }

            //************************* Utilities *************************//

            constexpr void clear() noexcept {
                container_.clear();
                if constexpr (is_stable()) counter_ = 0U;
                if constexpr (is_mutable()) id_to_pos_map_.clear();
                if constexpr (reuse_id()) free_id_pool_.clear();
                if constexpr (track_gen()) id_to_gen_map_.clear();
            }

            constexpr void swap(self_type_& other)
                noexcept(
                    std::is_nothrow_swappable_v<container_type> &&
                    std::is_nothrow_swappable_v<stored_compare_> &&
                    std::is_nothrow_swappable_v<position_map_type_> &&
                    std::is_nothrow_swappable_v<free_id_pool_type_> &&
                    std::is_nothrow_swappable_v<generation_map_type_>
                ) {
                using std::swap;
                swap(this->container_, other.container_);
                swap(this->comp_, other.comp_);
                if constexpr (is_stable()) swap(this->counter_, other.counter_);
                if constexpr (is_mutable()) swap(this->id_to_pos_map_, other.id_to_pos_map_);
                if constexpr (reuse_id()) swap(this->free_id_pool_, other.free_id_pool_);
                if constexpr (track_gen()) swap(this->id_to_gen_map_, other.id_to_gen_map_);
            }

            friend constexpr void swap(self_type_& lhs, self_type_& rhs)
                noexcept(noexcept(lhs.swap(rhs))) {
                lhs.swap(rhs);
            }

            // Compare sizes first, then the sorted content of the two heaps in O(nlogn) time
            [[nodiscard]] friend constexpr bool operator==(const self_type_& lhs, const self_type_& rhs) {
                if (lhs.size() != rhs.size())
                    return false;
                if (lhs.empty() && rhs.empty()) [[unlikely]]
                        return true;

                self_type_ lhs_copy = lhs, rhs_copy = rhs;
                auto& lhs_comp = lhs.value_comp();
                [[maybe_unused]] auto& rhs_comp = rhs.value_comp();
                while (!lhs_copy.empty()) {
                    auto top_eq = !std::invoke(lhs_comp, lhs_copy.top(), rhs_copy.top()) &&
                                  !std::invoke(lhs_comp, rhs_copy.top(), lhs_copy.top());

                    UL_ASSERT(
                        top_eq == (!rhs_comp(lhs_copy.top(), rhs_copy.top()) &&
                            !rhs_comp(rhs_copy.top(), lhs_copy.top())),
                        "The two comparators from the heaps do not give consistent results"
                    );

                    if (!top_eq) {
                        return false;
                    }
                    lhs_copy.unchecked_pop();
                    rhs_copy.unchecked_pop();
                }
                return true;
            }

            // Compare sizes first, then the sorted content of the two heaps using weak ordering in O(nlogn) time
            [[nodiscard]] friend constexpr auto operator<=>(const self_type_& lhs, const self_type_& rhs) {
                auto size1 = lhs.size(), size2 = rhs.size();
                if (size1 != size2) {
                    if (size1 < size2)
                        return std::weak_ordering::less;
                    return std::weak_ordering::greater;
                }

                if (lhs.empty() && rhs.empty()) [[unlikely]]
                        return std::weak_ordering::equivalent;

                self_type_ lhs_copy = lhs, rhs_copy = rhs;
                auto& lhs_comp = lhs_copy.value_comp();
                [[maybe_unused]] auto& rhs_comp = rhs.value_comp();

                auto do_comp = [&lhs_comp, &rhs_comp](const auto& lhs_top, const auto& rhs_top) {
                    bool res = std::invoke(lhs_comp, lhs_top, rhs_top);
                    UL_ASSERT(
                        res == rhs_comp(lhs_top, rhs_top),
                        "The two comparators of the heaps do not give consistent results"
                    );
                    return res;
                };

                while (!lhs_copy.empty()) {
                    if (do_comp(lhs_copy.top(), rhs_copy.top())) {
                        return std::weak_ordering::less;
                    }
                    if (do_comp(rhs_copy.top(), lhs_copy.top())) {
                        return std::weak_ordering::greater;
                    }

                    lhs_copy.unchecked_pop();
                    rhs_copy.unchecked_pop();
                }
                return std::weak_ordering::equivalent;
            }

        protected:
            // Data member
            container_type container_{};
            UL_NO_UNIQUE_ADDRESS stored_compare_ comp_{};
            [[maybe_unused]] UL_NO_UNIQUE_ADDRESS maybe_counter_type_ counter_{};
            [[maybe_unused]] UL_NO_UNIQUE_ADDRESS position_map_type_ id_to_pos_map_{};
            [[maybe_unused]] UL_NO_UNIQUE_ADDRESS free_id_pool_type_ free_id_pool_{};
            [[maybe_unused]] UL_NO_UNIQUE_ADDRESS generation_map_type_ id_to_gen_map_{};
        };
    }
}


#endif //URLICHT_D_ARY_HEAP_H
