#ifndef URLICHT_ADAPTIVE_MOVE_ONLY_FUNCTION_H
#define URLICHT_ADAPTIVE_MOVE_ONLY_FUNCTION_H

#include <urlicht/functional/detail/adaptive_functions_fwd.h>
#include <urlicht/functional/detail/adaptive_function_base.h>

namespace urlicht {
#define ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(QUALIFIERS, IS_CONST, IS_LVALUE, IS_RVALUE, IS_NOEXCEPT) \
    template <typename Ret, size_t OptimizeForSize, size_t OptimizeForAlign, typename ...Args> \
    class adaptive_move_only_function<Ret(Args...) QUALIFIERS, OptimizeForSize, OptimizeForAlign> \
        : public functional_detail::adaptive_function_base< \
            Ret(Args...), OptimizeForSize, OptimizeForAlign, IS_CONST, IS_LVALUE, \
            IS_RVALUE, IS_NOEXCEPT, false> { \
        using Base = functional_detail::adaptive_function_base< \
            Ret(Args...), OptimizeForSize, OptimizeForAlign, IS_CONST, IS_LVALUE, \
            IS_RVALUE, IS_NOEXCEPT, false>; \
    public: \
        using Base::Base; \
        using Base::operator=; \
        using result_type = Ret; \
    };

    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(, false, false, false, false)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(const, true, false, false, false)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(&, false, true, false, false)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(&&, false, false, true, false)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(noexcept, false, false, false, true)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(const &, true, true, false, false)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(const &&, true, false, true, false)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(const noexcept, true, false, false, true)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(& noexcept, false, true, false, true)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(&& noexcept, false, false, true, true)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(const & noexcept, true, true, false, true)
    ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC(const && noexcept, true, false, true, true)

#undef ADAPTIVE_MOVE_ONLY_FUNCTION_SPEC

}

#endif //URLICHT_ADAPTIVE_MOVE_ONLY_FUNCTION_H
