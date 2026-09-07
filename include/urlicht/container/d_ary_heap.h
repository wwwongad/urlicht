#ifndef URLICHT_D_ARY_HEAP_H
#define URLICHT_D_ARY_HEAP_H

#include <urlicht/container/detail/d_ary_heap_base_.h>
#include <urlicht/container/detail/d_ary_heap_mixin_.h>

namespace urlicht::container {

    /**
    * @brief A generic d-ary heap implementation with customizable arity, stability, mutability,
    *        container, and comparator. Zero overhead abstraction is enforced across different policies.
    *
    * @tparam T The type of elements stored in the heap. Must satisfy std::is_object_v.
    * @tparam Container A template template parameter representing the underlying storage container,
    *         position map, free id pool, and generation map (if applicable).
    *         It must accept a single type parameter corresponding to the heap value type.
    *         - Note: You can create a template alias to configure template parameters other than the first one,
    *         e.g. template <typename T> using heap_vec = std::vector<T, urlicht::memory::resource_view<T>>.
    *
    * @tparam Comp Comparison functor used to order elements. Defaults to std::less<>, yielding a max heap.
    * @tparam HeapPolicy A policy object of type d_ary_heap_policy that defines several aspects of the heap:
    *         - arity: branching factor (number of children per node). Defaults to 4.
    *         - mutable_: whether handles for accessing the elements are provided.
    *         - stable: whether insertion order is preserved for equivalent keys.
    *         - reuse_id: whether to maintain a free id pool for the reuse of ids in mutable heap.
    *         - track_generation: whether to keep track of the generation of ids to avoid [seemingly valid]
    *           dangling handles. Only applicable if reuse_id is true.
    * @tparam StabilityCounter Integral type used to track insertion order when stability is enabled.
    *         Defaults to std::uint64_t.
    */
    template <
        typename T,
        template <typename> typename Container = std::vector,
        typename Comp = std::less<>,
        d_ary_heap_policy HeapPolicy = d_ary_heap_policy{},
        typename StabilityCounter = std::uint64_t
    >
    class d_ary_heap final :
        /**
         * The reason why we use a base class is IDE friendliness. By including all common member methods in the
         * common base class and conditional member methods in separate mixin classes, we can hide the conditional
         * methods that are non-viable, by specializations of the mixin classes.
         *
         * If we instead include everything in one class, all methods will be shown by the IDE even though they
         * are non-viable (requires clause evaluated to false).
         */
        public detail::d_ary_heap_track_gen_mixin_<
            // Linear chain of public inheritance:
            // d_ary_heap -> d_ary_heap_track_gen_mixin_ -> d_ary_heap_reuse_id_mixin_ ->
            // d_ary_heap_mutable_mixin_ -> d_ary_heap_emplace_mixin_ ->
            // d_ary_heap_stable_mixin_ -> d_ary_heap_base_
            detail::d_ary_heap_base_<T, Container, Comp, HeapPolicy, StabilityCounter>,
            HeapPolicy.stable,
            HeapPolicy.mutable_,
            HeapPolicy.mutable_ && HeapPolicy.reuse_id,
            HeapPolicy.mutable_ && HeapPolicy.reuse_id && HeapPolicy.track_generation> {
        using common_base_ = detail::d_ary_heap_base_<T, Container, Comp, HeapPolicy, StabilityCounter>;
        using direct_base_ = detail::d_ary_heap_track_gen_mixin_<
            detail::d_ary_heap_base_<T, Container, Comp, HeapPolicy, StabilityCounter>,
            HeapPolicy.stable,
            HeapPolicy.mutable_,
            HeapPolicy.mutable_ && HeapPolicy.reuse_id,
            HeapPolicy.mutable_ && HeapPolicy.reuse_id && HeapPolicy.track_generation>;
    public:
        using value_type = typename common_base_::value_type;
        using reference = typename common_base_::reference;
        using const_reference = typename common_base_::const_reference;
        using pointer = typename common_base_::pointer;
        using const_pointer = typename common_base_::const_pointer;

        using container_type = typename common_base_::container_type;
        using size_type = typename common_base_::size_type;
        using difference_type = typename common_base_::difference_type;

        using counter_type = typename common_base_::counter_type;
        using id_type = typename common_base_::id_type;
        using value_compare = typename common_base_::value_compare;

        using const_iterator = typename common_base_::const_iterator;
        using const_reverse_iterator = typename common_base_::const_reverse_iterator;
        using handle_type = typename common_base_::handle_type;

        using direct_base_::direct_base_;
        using direct_base_::operator=;
    };

    namespace detail {
        template <typename>
        struct is_d_ary_heap : std::false_type {};

        template <typename T, template <typename> typename Cont,
                  typename Cmp, d_ary_heap_policy Policy, typename Cnt>
        struct is_d_ary_heap<d_ary_heap<T, Cont, Cmp, Policy, Cnt>> : std::true_type {};

        template <typename, typename>
        struct to_single_tparam_template;

        template <typename V, template <typename ...> typename Template, typename T, typename ...Args>
        struct to_single_tparam_template<V, Template<T, Args...>> {
            template <typename U>
            using template_type = Template<U, Args...>;

            static_assert(std::same_as<V, T>,
                "urlicht::container::priority_queue: the container's value_type must match the value type of "
                "the priority queue. This is to ensure the consistency of the extra template parameters.");
        };
    }

    //***************************** Type alias *****************************//

    // std::priority_queue compatible - the second parameter is a container type instead of template
    template <typename T, typename Cont = std::vector<T>, typename Cmp = std::less<>, size_t Arity = 4>
    using priority_queue =
        d_ary_heap<T,
            detail::to_single_tparam_template<T, Cont>::template template_type, Cmp, {.arity = Arity}>;

}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_d_ary_heap_v = container::detail::is_d_ary_heap<T>::value;
}

template <typename T,
          template <typename> typename Cont,
          typename Cmp,
          urlicht::container::d_ary_heap_policy Policy,
          typename Cnt,
          typename Alloc>
struct std::uses_allocator<urlicht::container::d_ary_heap<T, Cont, Cmp, Policy, Cnt>, Alloc>
    : std::bool_constant<
        std::uses_allocator_v<typename urlicht::container::d_ary_heap<T, Cont, Cmp, Policy, Cnt>::container_type, Alloc> &&
        (!Policy.mutable_ ||
            std::uses_allocator_v<Cont<typename urlicht::container::d_ary_heap<T, Cont, Cmp, Policy, Cnt>::size_type>, Alloc>) &&
        (!(Policy.mutable_ && Policy.reuse_id) ||
            std::uses_allocator_v<Cont<typename urlicht::container::d_ary_heap<T, Cont, Cmp, Policy, Cnt>::size_type>, Alloc>) &&
        (!(Policy.mutable_ && Policy.reuse_id && Policy.track_generation) ||
            std::uses_allocator_v<Cont<std::uint32_t>, Alloc>)
      >
{ };

#endif //URLICHT_D_ARY_HEAP_H
