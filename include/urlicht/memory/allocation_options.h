#ifndef URLICHT_MEMORY_ALLOCATION_OPTIONS_H
#define URLICHT_MEMORY_ALLOCATION_OPTIONS_H

namespace urlicht::memory {

    /**
     * @brief Compile-time options for memory resources (e.g. urlicht::memory::arena).
     *        Passed by value as a non-type template parameter, so designated initializers
     *        select the desired fields, e.g. arena<resource_options{.use_upstream = false}>.
     */
    struct resource_options {
        // Whether the resource falls back to its upstream allocator (heap chunks) once the
        // initial buffer cannot satisfy a request. When false, the resource serves only its
        // initial buffer and reports exhaustion as a failure instead of growing.
        bool use_upstream = true;
    };

    /**
     * @brief Compile-time options for allocator views and adapters (e.g.
     *        urlicht::memory::resource_view, urlicht::memory::pmr::arena_resource).
     *        Passed by value as a non-type template parameter.
     */
    struct allocator_options {
        // Whether allocation goes through the resource's unchecked, no-throw fast path
        // (unchecked_allocate) instead of the checked entry point. Requires the underlying
        // resource to expose that fast path.
        bool unchecked_allocate = false;
    };

} // namespace urlicht::memory

#endif //URLICHT_MEMORY_ALLOCATION_OPTIONS_H
