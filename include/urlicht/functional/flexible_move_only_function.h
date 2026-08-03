#ifndef URLICHT_FLEXIBLE_MOVE_ONLY_FUNCTION_H
#define URLICHT_FLEXIBLE_MOVE_ONLY_FUNCTION_H

#include <urlicht/functional/detail/flexible_functions_fwd.h>
#include <urlicht/functional/detail/flexible_function_base.h>
#include <memory_resource>

namespace urlicht::functional {
#define FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(QUALIFIERS, IS_CONST, IS_LVALUE, IS_RVALUE, IS_NOEXCEPT) \
    template <typename Ret, size_t OptimizeForSize, size_t OptimizeForAlign, typename Alloc, typename ...Args> \
    class flexible_move_only_function<Ret(Args...) QUALIFIERS, OptimizeForSize, OptimizeForAlign, Alloc> \
        : public detail::flexible_function_call_operator_< \
            Ret(Args...), OptimizeForSize, OptimizeForAlign, IS_CONST, IS_LVALUE, \
            IS_RVALUE, IS_NOEXCEPT, false, Alloc> { \
        using base_t = detail::flexible_function_call_operator_< \
            Ret(Args...), OptimizeForSize, OptimizeForAlign, IS_CONST, IS_LVALUE, \
            IS_RVALUE, IS_NOEXCEPT, false, Alloc>; \
    public: \
        using base_t::base_t; \
        using base_t::operator=; \
        using result_type = Ret; \
        using allocator_type = Alloc; \
        friend constexpr void swap(flexible_move_only_function& lhs, flexible_move_only_function& rhs)\
        noexcept(noexcept(lhs.swap(rhs))) { \
            lhs.swap(rhs); \
        } \
    };

    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(, false, false, false, false)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(const, true, false, false, false)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(&, false, true, false, false)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(&&, false, false, true, false)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(noexcept, false, false, false, true)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(const &, true, true, false, false)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(const &&, true, false, true, false)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(const noexcept, true, false, false, true)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(& noexcept, false, true, false, true)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(&& noexcept, false, false, true, true)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(const & noexcept, true, true, false, true)
    FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC(const && noexcept, true, false, true, true)

#undef FLEXIBLE_MOVE_ONLY_FUNCTION_SPEC

    namespace pmr {
        template <typename T,
                  size_t OptimizeForSize = 64u,
                  size_t OptimizeForAlign = alignof(std::max_align_t)>
        using flexible_move_only_function =
            urlicht::functional::flexible_move_only_function<
                T, OptimizeForSize, OptimizeForAlign, std::pmr::polymorphic_allocator<>
            >;
    }
}

#endif //URLICHT_FLEXIBLE_MOVE_ONLY_FUNCTION_H
