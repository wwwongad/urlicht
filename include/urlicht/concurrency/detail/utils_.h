#ifndef URLICHT_CONCURRENCY_DETAIL_UTILS__H
#define URLICHT_CONCURRENCY_DETAIL_UTILS__H

#include <new>
#include <algorithm>
#include <cinttypes>

#if defined(__x86_64__) || defined(__i386__)
#include <xmmintrin.h>
#endif

namespace urlicht::concurrency {
    template <std::uint64_t N>
    using capacity = std::integral_constant<std::uint64_t, N>;

    namespace detail {
        inline constexpr auto cacheline_size_ =
            std::max(
                std::hardware_destructive_interference_size,
                static_cast<decltype(std::hardware_destructive_interference_size)>(64U)
            );

        template <typename>
        struct is_capacity_ : std::false_type {};

        template <std::uint64_t N>
        struct is_capacity_<urlicht::concurrency::capacity<N>> : std::true_type {};

#if defined(__x86_64__) || defined(__i386__)

        inline void cpu_relax() { _mm_pause(); }

#elif defined(__PPC__)

        inline void cpu_relax() { __asm__ volatile("yield"); }

#elif defined(__s390x__) || defined(__zarch__)

        inline void cpu_relax() { }

#elif defined(__aarch64__)

        inline void cpu_relax() { __asm__ volatile("yield"); }

#elif defined(__riscv)

        inline void cpu_relax() { __asm__ volatile(".int 0x0100000F" : : : "memory"); }

#else
        inline void cpu_relax() { }
#warning "Using an empty cpu_relax() for this architecture"
#endif
    }
}
#endif //URLICHT_CONCURRENCY_DETAIL_UTILS__H
