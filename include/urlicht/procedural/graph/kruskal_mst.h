#ifndef URLICHT_KRUSKAL_MST_H
#define URLICHT_KRUSKAL_MST_H
#include <type_traits>
#include <urlicht/adaptor/union_find.hpp>
#include <urlicht/concepts_utility.h>
#include <urlicht/compare.h>

namespace urlicht::procedural {

    namespace detail {

        template <typename Edge>
        concept edge =
            requires(const Edge& e) { e.u; e.v; }
        &&  std::same_as<
                std::remove_cvref_t<decltype(std::declval<Edge>().u)>,
                std::remove_cvref_t<decltype(std::declval<Edge>().v)>
            >
        &&  (requires(const Edge& e) { { e.w }; }
                &&  concepts::real_number<std::remove_cvref_t<decltype(std::declval<Edge>().w)>>
            ||
            (requires(const Edge& e) { { e.weight }; }
                &&  concepts::real_number<std::remove_cvref_t<decltype(std::declval<Edge>().weight)>>)
            );

        template <edge Edge>
        using vertex_t = std::remove_cvref_t<decltype(std::declval<Edge>().v)>;

        template <edge Edge>
        struct weight_traits {
            static constexpr auto get_weight(const Edge& e) {
                if constexpr (requires (const Edge& e) { { e.w }; }) {
                    return e.w;
                } else {
                    return e.weight;
                }
            }
            using type = std::remove_cvref_t<decltype(get_weight(std::declval<Edge>()))>;
        };

        template <edge Edge>
        using weight_t = typename weight_traits<Edge>::type;

        inline constexpr std::size_t TRACKSIZE_THRESHOLD = 1024; // Tracks size if HintN >= threshold

        template <std::size_t N>
        constexpr bool track_size_v = N >= TRACKSIZE_THRESHOLD;

    }

    /**
     *@brief Implements the Kruskal's MST algorithm in-place.
     *       The input container is sorted by weight, and non-MST edges are erased.
     *       If you want to keep the input unchanged, use non-inplace version.
     *
     *@tparam HintN: An approximate upper limit of the number of vertices for
     *               possible compile time optimizations. Optional.
     *
     *@param edges: A generic random access container whose elements represent edges and
     *              must have public data member u, v and w/weight. u, v must be of the
     *              same type and hashable. w/weight must be a real number type.
     *
     *@param cmp: Custom less comparator. Defaults to urlicht::compare::less<>
     *
     *@return: The total weight of the MST
     *
     *@warning: Do NOT include any negative vertex if it is signed integral. If you must, set
     *          tparam Uf to map_union_find<int>
     */
    template <std::size_t HintN = detail::TRACKSIZE_THRESHOLD,
              concepts::random_access_container Cont,
              concepts::comparison_functor<detail::weight_t<typename Cont::value_type>> Cmp = compare::less<>,
              concepts::basic_union_find Uf = basic_union_find<detail::vertex_t<typename Cont::value_type>,
                                                               detail::track_size_v<HintN>>
    >
    requires detail::edge<typename Cont::value_type>
    constexpr auto kruskal_mst_inplace(Cont& edges, Cmp cmp = Cmp{}) {
        using Edge = typename Cont::value_type;
        // Uses array-based uf for int. Map-based uf otherwise

        std::ranges::sort(edges, [&cmp](const auto& e1, const auto& e2) {
            return cmp(detail::weight_traits<Edge>::get_weight(e1),
                       detail::weight_traits<Edge>::get_weight(e2));
        });

        Uf uf;
        if constexpr (requires(Uf x, typename Uf::size_type n) { x.reserve(n); }) {
            // Best-effort reservation: assumes the vertices are compact
            uf.reserve(edges.size());
        }

        detail::weight_t<Edge> mst_weight{};
        std::size_t write = 0;
        for (std::size_t read = 0; read < edges.size(); ++read) {
            if (const auto& e = edges[read]; !uf.same_set(e.u, e.v)) {
                uf.unite_new(e.u, e.v); // Inserts new vertices automatically
                mst_weight += detail::weight_traits<Edge>::get_weight(e);
                if (write != read) {
                    edges[write] = std::move(edges[read]);
                }
                ++write;
            }
        }
        edges.erase(edges.begin() + static_cast<std::ptrdiff_t>(write), edges.end());
        return mst_weight;
    }

    /**
     * @brief Implements the Kruskal's MST algorithm without modifying the input container.
     *        A copy of the input edges is made, sorted by weight, and reduced to the MST edges.
     *        Returns both the total weight of the MST and the resulting edge set.
     *
     * @tparam HintN An approximate upper limit of the number of vertices for
     *               possible compile-time optimizations. Optional.
     *
     * @param edges A generic random access container whose elements represent edges and
     *              must have public data members u, v and w/weight. u and v must be of the
     *              same type and hashable. w/weight must be a real number type.
     *
     * @param cmp   Custom less comparator. Defaults to urlicht::compare::less<>.
     *
     * @return A std::pair consisting of:
     *         - The total weight of the MST.
     *         - A std::vector<Edge> containing only the MST edges.
     *
     * @note This is generally less efficient than kruskal_mst_inplace(). Use only when you
     *       really do not want to modify the input container.
     *
     * @warning: Do NOT include any negative vertex if it is signed integral. If you must, set
     *           tparam Uf to map_union_find<int>
     */
    template <std::size_t HintN = detail::TRACKSIZE_THRESHOLD,
              concepts::random_access_container Cont,
              concepts::comparison_functor<detail::weight_t<typename Cont::value_type>> Cmp = compare::less<>,
              concepts::basic_union_find Uf = basic_union_find<detail::vertex_t<typename Cont::value_type>,
                                                               detail::track_size_v<HintN>>
    >
    requires detail::edge<typename Cont::value_type>
    constexpr auto kruskal_mst(const Cont& edges, Cmp cmp = Cmp{}) {
        using Edge = std::ranges::range_value_t<Cont>;
        std::vector<Edge> copy(edges);
        auto w = kruskal_mst_inplace<HintN, Cont, Cmp, Uf>(copy, cmp);
        return std::make_pair(w, std::move(copy));
    }

}

#endif //URLICHT_KRUSKAL_MST_H
