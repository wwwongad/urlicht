#ifndef URLICHT_TEST_CONTAINER_UTILS_HEAP_CHECKS_H
#define URLICHT_TEST_CONTAINER_UTILS_HEAP_CHECKS_H

#include <gtest/gtest.h>
#include <urlicht/container/detail/is_d_ary_heapified_.h>
#include <algorithm>
#include <ranges>

template <typename Heap>
bool is_d_ary_heapified(Heap heap) { // Intentionally copied
    return urlicht::container::detail::is_d_ary_heapified_<Heap::arity()>(
         std::ranges::subrange{ heap.begin(), heap.end() }, heap.value_comp()
    );
}

template <typename Heap>
bool is_heap_of(Heap heap, std::vector<typename Heap::value_type> vec) { // Intentionally copied
    std::ranges::sort(vec, heap.value_comp());  // ensure the vec is sorted (reversed relative to the heap)
    EXPECT_EQ(heap.size(), vec.size());

    while (!heap.empty()) {
        auto res = !(heap.value_comp()(heap.top(), vec.back()))
                && !(heap.value_comp()(vec.back(), heap.top()));
        EXPECT_TRUE(res);
        if (!res) {
            return false;
        }
        heap.pop();
        vec.pop_back();
    }
    return true;
}

#endif //URLICHT_TEST_CONTAINER_UTILS_HEAP_CHECKS_H
