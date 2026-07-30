#ifndef URLICHT_ALLOCATOR_TYPE_H
#define URLICHT_ALLOCATOR_TYPE_H

#include <urlicht/concepts/concepts.h>

namespace urlicht::internal {

    // std::conditional_t requires both branches to be valid expressions, which might not be the case here.
    template <typename, bool>
    struct allocator_type_impl_ {
        using type = void;
    };

    template <typename Cont>
    struct allocator_type_impl_ <Cont, true> {
        using type = typename Cont::allocator_type;
    };

    template <typename Cont>
    using allocator_of_t = allocator_type_impl_<Cont, urlicht::concepts::has_allocator<Cont>>::type;
}

#endif //URLICHT_ALLOCATOR_TYPE_H
