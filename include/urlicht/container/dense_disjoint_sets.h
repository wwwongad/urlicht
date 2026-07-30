#ifndef URLICHT_DENSE_DISJOINT_SETS_H
#define URLICHT_DENSE_DISJOINT_SETS_H

#include <urlicht/config.h>
#include <urlicht/concepts/concepts.h>
#include <urlicht/internal/scope_guard.h>
#include <algorithm>
#include <limits>
#include <optional>
#include <type_traits>
#include <variant>
#include <vector>
#include <numeric> // iota

namespace urlicht::container {

    enum class dense_disjoint_sets_union_policy : std::uint8_t {
        none,
        by_size,
        by_rank
    };

    enum class dense_disjoint_sets_path_policy : std::uint8_t {
        none,
        path_halving,
        full_compression
    };

    struct dense_disjoint_sets_policy {
        dense_disjoint_sets_union_policy union_by = dense_disjoint_sets_union_policy::by_size;
        dense_disjoint_sets_path_policy path = dense_disjoint_sets_path_policy::path_halving;
    };

    /**
     * @brief Dense union-find over the universe [0, n) with compile-time union and path policies.
     *
     * @tparam Id Unsigned integral id type. It must be an integral type smaller or equal to Container::size_type
     *            to avoid waste of memory. Defaults to std::uint32_t.
     * @tparam Policy Compile-time union-find policy. Uses size-based union and path halving by default.
     * @tparam ParentContainer Single-parameter contiguous container template used for internal storage of parents.
     * @tparam MetricContainer Single-parameter contiguous container template used for internal storage of metrics.
     */
    template <
        std::unsigned_integral Id = std::uint32_t,
        dense_disjoint_sets_policy Policy = dense_disjoint_sets_policy{},
        template <typename> typename ParentContainer = std::vector,
        template <typename> typename MetricContainer = ParentContainer
    >
    class dense_disjoint_sets final {
        using self_type_ = dense_disjoint_sets;

    public:
        // Static reflections

        [[nodiscard]] static consteval dense_disjoint_sets_union_policy union_policy() noexcept {
            return Policy.union_by;
        }
        [[nodiscard]] static consteval dense_disjoint_sets_path_policy path_policy() noexcept {
            return Policy.path;
        }

        [[nodiscard]] static consteval bool unite_by_size() noexcept {
            return Policy.union_by == dense_disjoint_sets_union_policy::by_size;
        }
        [[nodiscard]] static consteval bool unite_by_rank() noexcept {
            return Policy.union_by == dense_disjoint_sets_union_policy::by_rank;
        }
        [[nodiscard]] static consteval bool uses_union_heuristic() noexcept {
            return unite_by_size() || unite_by_rank();
        }

        [[nodiscard]] static consteval bool uses_path_halving() noexcept {
            return path_policy() == dense_disjoint_sets_path_policy::path_halving;
        }
        [[nodiscard]] static consteval bool uses_full_compression() noexcept {
            return path_policy() == dense_disjoint_sets_path_policy::full_compression;
        }
        [[nodiscard]] static consteval bool compresses_paths() noexcept {
            return path_policy() != dense_disjoint_sets_path_policy::none;
        }

        using id_type = Id;
        using value_type = Id;
        using reference = value_type&;
        using const_reference = const value_type&;
        using pointer = value_type*;
        using const_pointer = const value_type*;

        using parent_container_type = ParentContainer<id_type>;
        using container_type = parent_container_type;
        using size_type = typename parent_container_type::size_type;
        using difference_type = typename parent_container_type::difference_type;

        using counter_type = size_type;
        using metric_type = std::conditional_t<unite_by_size(), size_type, id_type>;
        using metric_container_type =
            std::conditional_t<uses_union_heuristic(), MetricContainer<metric_type>, std::monostate>;

        using const_iterator = typename parent_container_type::const_iterator;
        using const_reverse_iterator = typename parent_container_type::const_reverse_iterator;

        static_assert(sizeof(id_type) <= sizeof(size_type),
                      "id_type should not be a greater integral type than size_type.");
        static_assert(urlicht::concepts::contiguous_container<parent_container_type>,
                      "Container must model concept contiguous_container.");
        static_assert(!uses_union_heuristic() || urlicht::concepts::contiguous_container<metric_container_type>,
                      "Container must model concept contiguous_container.");

    private:
        template <typename Alloc>
        static constexpr bool uses_allocator_ =
            std::uses_allocator_v<parent_container_type, Alloc> &&
            (!uses_union_heuristic() || std::uses_allocator_v<metric_container_type, Alloc>);

    public:
        //*************************** Constructors ***************************//

        constexpr dense_disjoint_sets()
        noexcept(std::is_nothrow_default_constructible_v<parent_container_type> &&
                 std::is_nothrow_default_constructible_v<metric_container_type>)
        requires std::default_initializable<container_type>
        = default;

        template <urlicht::concepts::allocator Alloc>
        requires uses_allocator_<Alloc>
        constexpr explicit dense_disjoint_sets(const Alloc& alloc)
        : parents_{std::make_obj_using_allocator<parent_container_type>(alloc)},
          metrics_{std::make_obj_using_allocator<metric_container_type>(alloc)}
        {   }

        /**
         * @brief Construct a forest with ids [0, n), each initially its own set.
         */
        constexpr explicit dense_disjoint_sets(const size_type n) {
            append_sets(n);
        }

        template <urlicht::concepts::allocator Alloc>
        requires uses_allocator_<Alloc>
        constexpr dense_disjoint_sets(const size_type n, const Alloc& alloc)
        : parents_{std::make_obj_using_allocator<parent_container_type>(alloc)},
          metrics_{std::make_obj_using_allocator<metric_container_type>(alloc)} {
            append_sets(n);
        }

        constexpr dense_disjoint_sets(const dense_disjoint_sets&) = default;
        constexpr dense_disjoint_sets& operator=(const dense_disjoint_sets&) = default;

        constexpr dense_disjoint_sets(dense_disjoint_sets&& other)
        noexcept(std::is_nothrow_move_constructible_v<parent_container_type> &&
                 std::is_nothrow_move_constructible_v<metric_container_type>)
        try : parents_{std::move(other.parents_)},
              metrics_{std::move(other.metrics_)},
              set_count_{other.set_count_} {
            other.clear();
        }
        catch (...) {
            other.clear();
            if constexpr (!std::is_nothrow_move_constructible_v<dense_disjoint_sets>) {
                throw;
            }
        }

        constexpr dense_disjoint_sets& operator=(dense_disjoint_sets&& other)
        noexcept(std::is_nothrow_move_assignable_v<parent_container_type> &&
                 std::is_nothrow_move_assignable_v<metric_container_type>) {
            if (this == &other) [[unlikely]] {
                return *this;
            }
            auto other_other_guard = urlicht::internal::make_scope_guard([&] { other.clear(); }); // always clear other
            auto do_move = [&] {
                this->parents_ = std::move(other.parents_);
                this->metrics_ = std::move(other.metrics_);
                this->set_count_ = other.set_count_;
            };
            if constexpr (std::is_nothrow_move_assignable_v<dense_disjoint_sets>) {
                do_move();
            } else {
                auto clear_this_guard = urlicht::internal::make_scope_guard([&] { this->clear(); });
                do_move();
                clear_this_guard.release();
            }
            return *this;
        }

        //*********************** Core Methods ************************//

        /**
         * @brief Find the representative of x, applying the configured path policy.
         */
        [[nodiscard]] constexpr id_type find(const id_type x) {
            if (!contains(x)) {
                throw std::out_of_range{"dense_disjoint_sets::find: x is out of range"};
            }
            return unchecked_find(x);
        }

        /**
         * @brief Find the representative of x without mutating the forest.
         */
        [[nodiscard]] constexpr id_type find(const id_type x) const {
            if (!contains(x)) {
                throw std::out_of_range{"dense_disjoint_sets::find: x is out of range"};
            }
            return unchecked_find(x);
        }

        /**
         * @brief Find the representative of x if x is valid, applying the configured path policy.
         */
        [[nodiscard]] constexpr std::optional<id_type> try_find(const id_type x) noexcept {
            if (!contains(x)) [[unlikely]] {
                return std::nullopt;
            }
            return std::make_optional(unchecked_find(x));
        }

        /**
         * @brief Find the representative of x if x is valid without mutating the forest.
         */
        [[nodiscard]] constexpr std::optional<id_type> try_find(const id_type x) const noexcept {
            if (!contains(x)) [[unlikely]] {
                return std::nullopt;
            }
            return std::make_optional(unchecked_find(x));
        }

        /**
         * @brief Find the representative of x without bounds checks, applying the configured path policy.
         * @pre x < size().
         */
        [[nodiscard]] constexpr id_type unchecked_find(const id_type x) noexcept {
            return find_mut_(x);
        }

        /**
         * @brief Find the representative of x without bounds checks and without mutating the forest.
         * @pre x < size().
         */
        [[nodiscard]] constexpr id_type unchecked_find(const id_type x) const noexcept {
            return find_root_no_compress_(x);
        }

        /**
         * @brief Check whether x and y belong to the same set without bounds checks.
         * @pre x < size() and y < size().
         */
        [[nodiscard]] constexpr bool unchecked_same_set(const id_type x, const id_type y) noexcept {
            return find_mut_(x) == find_mut_(y);
        }

        /**
         * @brief Check whether x and y belong to the same set.
         */
        [[nodiscard]] constexpr bool same_set(const id_type x, const id_type y) noexcept {
            if (!contains(x) || !contains(y)) [[unlikely]] {
                return false;
            }
            return unchecked_same_set(x, y);
        }

        /**
         * @brief Check whether x and y belong to the same set without bounds checks.
         * @pre x < size() and y < size().
         */
        [[nodiscard]] constexpr bool unchecked_same_set(const id_type x, const id_type y) const noexcept {
            return find_root_no_compress_(x) == find_root_no_compress_(y);
        }

        /**
         * @brief Check whether x and y belong to the same set.
         */
        [[nodiscard]] constexpr bool same_set(const id_type x, const id_type y) const noexcept {
            if (!contains(x) || !contains(y)) [[unlikely]] {
                return false;
            }
            return unchecked_same_set(x, y);
        }

        /**
         * @brief Unite the sets of x and y without bounds checks.
         * @pre x < size() and y < size().
         */
        constexpr void unchecked_unite(const id_type x, const id_type y) noexcept {
            UL_ASSERT(contains(x) && contains(y), "x or y out-of-range.");
            unite_roots_(find_mut_(x), find_mut_(y));
        }

        /**
         * @brief Unite the sets of x and y if both ids are valid. Returns false if at least one of the ids
         *        does not exist, or they already belong to the same set.
         */
        constexpr bool try_unite(const id_type x, const id_type y) noexcept {
            if (!contains(x) || !contains(y)) [[unlikely]] {
                return false;
            }
            return unite_roots_(find_mut_(x), find_mut_(y));
        }

        /**
         * @brief Unite the sets of x and y.
         * @throws std::out_of_range if x or y is out of range.
         */
        constexpr void unite(const id_type x, const id_type y) {
            if (!contains(x) || !contains(y)) [[unlikely]] {
                throw std::out_of_range("urlicht::container::dense_disjoint_sets::unite(): x or y out-of-range.");
            }
            unchecked_unite(x, y);
        }

        // Returns the newly created id.
        constexpr id_type make_set() {
            const auto old_size = size();
            if (old_size >= max_size()) [[unlikely]] {
                throw std::length_error("urlicht::container::dense_disjoint_sets::make_set(): max size exceeded.");
            }

            const id_type new_id = static_cast<id_type>(old_size);

            if constexpr (uses_union_heuristic()) {
                metrics_.emplace_back(initial_metric_value_());

                auto rollback_metrics = urlicht::internal::make_scope_guard([&]() noexcept {
                    metrics_.pop_back();
                });

                parents_.emplace_back(new_id);
                rollback_metrics.release();
            } else {
                parents_.emplace_back(new_id);
            }
            ++set_count_;
            return new_id;
        }

        /**
         * @brief Append {count} new singleton sets.
         * @return A half-open interval [first, last) indicating the range of ids appended.
         * @note Return type is a pair of size_type (not id_type) since it may overflow by 1
         */
        constexpr std::pair<size_type, size_type> append_sets(const size_type count) {
            if (count == 0) [[unlikely]] {
                return std::make_pair(size(), size());
            }

            if (count > max_size() - size()) [[unlikely]] {
                throw std::length_error{"urlicht::container::dense_disjoint_sets::append_sets(): size exceeds max size."};
            }

            const auto old_size = size();
            const auto new_size = old_size + count;
            reserve(new_size);

            if constexpr (uses_union_heuristic()) {
                // resize should have strong exception safety
                metrics_.resize(new_size, initial_metric_value_());
                auto rollback_metrics = urlicht::internal::make_scope_guard([&]() noexcept {
                    metrics_.resize(old_size);
                });

                parents_.resize(new_size);
                rollback_metrics.release();

                initialize_new_parents_(old_size, new_size);
            } else {
                parents_.resize(new_size);
                initialize_new_parents_(old_size, new_size);
            }
            set_count_ += count;
            return std::make_pair(old_size, new_size);
        }

        constexpr void grow_to(const size_type n) {
            if (n > size()) [[likely]] {
                append_sets(n - size());
            }
        }

        /**
         * @brief Reset the forest so every id becomes a singleton root.
         */
        constexpr void reset_all() noexcept {
            initialize_new_parents_(0, size());
            if constexpr (uses_union_heuristic()) {
                const auto init_metric = initial_metric_value_();
                std::fill(metrics_.begin(), metrics_.end(), init_metric);
            }
            set_count_ = size();
        }

        //*********************** Capacity ************************//

        constexpr void reserve(const size_type n) {
            if (n > max_size()) [[unlikely]] {
                throw std::length_error{"urlicht::container::dense_disjoint_sets::reserve(): n exceeds max_size()."};
            }
            parents_.reserve(n);
            if constexpr (uses_union_heuristic()) {
                metrics_.reserve(n);
            }
        }

        [[nodiscard]] constexpr size_type size() const noexcept {
            return parents_.size();
        }

        [[nodiscard]] constexpr bool empty() const noexcept {
            return parents_.empty();
        }

        [[nodiscard]] constexpr size_type capacity() const noexcept {
            if constexpr (uses_union_heuristic()) {
                return std::min(parents_.capacity(), metrics_.capacity());
            } else {
                return parents_.capacity();
            }
        }

        [[nodiscard]] constexpr size_type max_size() const noexcept {
            size_type max_size{};
            if constexpr (sizeof(size_type) == sizeof(id_type)) {
                max_size = std::numeric_limits<id_type>::max();
            } else {
                max_size = static_cast<size_type>(std::numeric_limits<id_type>::max()) + size_type{1U};
            }
            max_size = std::min(max_size, parents_.max_size());
            if constexpr (uses_union_heuristic()) {
                max_size = std::min(max_size, metrics_.max_size());
            }
            return max_size;
        }

        [[nodiscard]] constexpr id_type max_id() const noexcept {
            return static_cast<id_type>(max_size() - size_type{1U});  // ALWAYS max_size() - 1
        }

        /**
         * @brief Shrink internal storage to fit the current size when supported.
         */
        constexpr void shrink_to_fit() {
            if constexpr (urlicht::concepts::reservable_container<parent_container_type>) {
                parents_.shrink_to_fit();
            }
            if constexpr (uses_union_heuristic() && urlicht::concepts::reservable_container<metric_container_type>) {
                metrics_.shrink_to_fit();
            }
        }

        //*********************** Observers ************************//
        /**
         * @brief Check whether x is a valid id in the current universe.
         */
        [[nodiscard]] constexpr bool contains(const id_type idx) const noexcept {
            return static_cast<size_type>(idx) < size();
        }

        [[nodiscard]] constexpr bool is_root(const id_type idx) const noexcept {
            UL_ASSERT(idx < size(), "idx out-of-range");
            return parents_[idx] == idx;
        }

        [[nodiscard]] constexpr counter_type set_count() const noexcept {
            return set_count_;
        }

        [[nodiscard]] constexpr const parent_container_type& parents() const noexcept {
            return parents_;
        }

        [[nodiscard]] constexpr const metric_container_type& metrics() const noexcept
        requires (uses_union_heuristic()) {
            return metrics_;
        }

        // Returns the immediate parent of the given id
        [[nodiscard]] constexpr id_type operator[](const id_type idx) const noexcept {
            UL_ASSERT(idx < size(), "idx out-of-range.");
            return parents_[idx];
        }

        //*********************** Iterator ************************//

        [[nodiscard]] constexpr const_iterator cbegin() const noexcept {
            return parents_.cbegin();
        }
        [[nodiscard]] constexpr const_iterator cend() const noexcept {
            return parents_.cend();
        }
        [[nodiscard]] constexpr const_iterator begin() const noexcept {
            return cbegin();
        }
        [[nodiscard]] constexpr const_iterator end() const noexcept {
            return cend();
        }

        [[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept {
            return parents_.crbegin();
        }
        [[nodiscard]] constexpr const_reverse_iterator crend() const noexcept {
            return parents_.crend();
        }
        [[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept {
            return crbegin();
        }
        [[nodiscard]] constexpr const_reverse_iterator rend() const noexcept {
            return crend();
        }

        //*********************** Utilities ************************//

        constexpr void clear() noexcept {
            parents_.clear();
            if constexpr (uses_union_heuristic()) {
                metrics_.clear();
            }
            set_count_ = 0U;
        }

        constexpr void swap(self_type_& other)
        noexcept(std::is_nothrow_swappable_v<parent_container_type> &&
                 std::is_nothrow_swappable_v<metric_container_type> &&
                 std::is_nothrow_swappable_v<size_type>) {
            using std::swap;
            swap(this->parents_, other.parents_);
            if constexpr (uses_union_heuristic()) {
                swap(this->metrics_, other.metrics_);
            }
            swap(this->set_count_, other.set_count_);
        }

        friend constexpr void swap(self_type_& lhs, self_type_& rhs)
            noexcept(noexcept(lhs.swap(rhs))) {
            lhs.swap(rhs);
        }

        // Returns true if the two disjoint sets have the exact same forest state.
        friend constexpr bool operator==(const self_type_& lhs, const self_type_& rhs) noexcept {
            if (lhs.set_count_ != rhs.set_count_) {
                return false;
            }

            if (lhs.parents_ != rhs.parents_) {
                return false;
            }
            return true;  // If parent containers are equal, metric containers must also be
        }

        // First compares set_count(), then compares parents(), using the three-way comparator of
        // parent_container_type.
        friend constexpr std::strong_ordering operator<=>(const self_type_& lhs, const self_type_& rhs) noexcept {
            if (lhs.set_count_ != rhs.set_count_) {
                return lhs.set_count_ <=> rhs.set_count_;
            }

            if (lhs.parents_ != rhs.parents_) {
                return lhs.parents_ <=> rhs.parents_;
            }
            return std::strong_ordering::equal;
        }

    private:
        [[nodiscard]] static constexpr metric_type initial_metric_value_() noexcept
        requires (uses_union_heuristic()) {
            if constexpr (unite_by_size()) {
                return static_cast<metric_type>(1);
            } else {
                return static_cast<metric_type>(0);
            }
        }

        constexpr void initialize_new_parents_(const size_type first, const size_type last) noexcept {
            std::iota(parents_.begin() + first, parents_.begin() + last, static_cast<id_type>(first));
        }

        [[nodiscard]] constexpr id_type find_root_no_compress_(id_type x) const noexcept {
            while (parents_[x] != x) {
                x = parents_[x];
            }
            return x;
        }

        [[nodiscard]] constexpr id_type find_mut_(const id_type x) noexcept {
            if constexpr (uses_path_halving()) {
                id_type curr = x;
                while (parents_[curr] != curr) {
                    parents_[curr] = parents_[parents_[curr]];
                    curr = parents_[curr];
                }
                return curr;
            } else if constexpr (uses_full_compression()) {
                id_type root = find_root_no_compress_(x);

                id_type curr = x;
                while (parents_[curr] != curr) {
                    const id_type parent = parents_[curr];
                    parents_[curr] = root;
                    curr = parent;
                }
                return root;
            } else {
                return find_root_no_compress_(x);
            }
        }

        constexpr bool unite_roots_(id_type x, id_type y) noexcept {
            if (x == y) [[unlikely]] {
                return false;
            }
            if constexpr (unite_by_size()) {
                if (metrics_[x] < metrics_[y]) {
                    parents_[x] = y;
                    metrics_[y] = metrics_[x];
                } else {
                    parents_[y] = x;
                    metrics_[x] += metrics_[y];
                }
            } else if constexpr (unite_by_rank()) {
                if (metrics_[x] < metrics_[y]) {
                    std::swap(x, y);
                }
                parents_[y] = x;
                metrics_[x] += (metrics_[x] == metrics_[y]);
            } else {
                parents_[y] = x;
            }
            --set_count_;
            return true;
        }

        // Data members
        parent_container_type parents_{};
        UL_NO_UNIQUE_ADDRESS metric_container_type metrics_{};
        counter_type set_count_{0U};
    };

    namespace detail {
        template <typename>
        struct is_dense_disjoint_sets : std::false_type {};

        template <std::unsigned_integral Id,
                  dense_disjoint_sets_policy P,
                  template <typename> typename PC,
                  template <typename> typename MC>
        struct is_dense_disjoint_sets<dense_disjoint_sets<Id, P, PC, MC>> : std::true_type {};
    }

    //*************************** CTAD Guides **************************//

    dense_disjoint_sets() -> dense_disjoint_sets<>;

    // If you use bool, this will be ill-formed
    template <std::integral IdT>
    dense_disjoint_sets(IdT) -> dense_disjoint_sets<std::make_unsigned_t<IdT>>;

} // namespace urlicht


// std::uses_allocator spec
template <std::unsigned_integral Id,
    urlicht::container::dense_disjoint_sets_policy P,
    template <typename> typename PC,
    template <typename> typename MC,
    typename Alloc>
struct std::uses_allocator<urlicht::container::dense_disjoint_sets<Id, P, PC, MC>, Alloc>
    : bool_constant<
        std::uses_allocator_v<PC<Id>, Alloc> &&
        (!urlicht::container::dense_disjoint_sets<Id, P, PC, MC>::uses_union_heuristic() ||
            std::uses_allocator_v<
                typename urlicht::container::dense_disjoint_sets<Id, P, PC, MC>::metric_container_type, Alloc>)
    >
{   };

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_dense_disjoint_sets_v =
        container::detail::is_dense_disjoint_sets<T>::value;
}

#endif // URLICHT_DENSE_DISJOINT_SETS_H
