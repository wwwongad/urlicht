# Urlicht - High Performance C++ Template Library

Urlicht is a modern header-only C++20 template library that extends the standard library with a variety of
infrastructural tools.

The library is designed for high performance, template configurability, and API completeness. It includes containers, 
algorithms, type-erased wrappers, concurrency primitives, and memory management tools. All public names live under the `urlicht` namespace.

## Contents

- [Highlights](#highlights)
- [Requirements](#requirements)
- [Modules](#modules)
  - [algorithm](#algorithm)
  - [any](#any)
  - [concepts](#concepts)
  - [concurrency](#concurrency)
  - [container](#container)
  - [functional](#functional)
  - [memory](#memory)
- [Usage](#usage)
- [Building and installing](#building-and-installing)
- [Testing, benchmarks, and examples](#testing-benchmarks-and-examples)
- [License](#license)

## Highlights
- **C++20 compatible** with optional C++23/26 features (`constexpr` upgrades, stacktrace, range adaptors) enabled automatically when the compiler supports them.
- **Performance-oriented** — lock-free queues, branchless binary search, custom SBO type erasure, SoA flat maps, bump arenas, and huge-page support. All rigorously benchmarked against the standard library and other open-source implementations.
- **Template Configurability** — policies - including storage model, SBO size, and additional features - fully configurable via template parameters, with strict zero-overhead abstraction - i.e., disabled features carry no runtime cost.
- **API Completeness** — full STL-like interface, standard traits specialization, error handling variants (unchecked/try/error_code/throwing methods), in-place modifiers, and fine-grained tag dispatchers.
- **Concept-hardened APIs** — every public template constrains its parameters with C++20 concepts and `static_assert`s that produce readable diagnostics.

## Requirements

- A C++20-capable compiler: GCC, Clang, or MSVC.
- CMake 3.30+ (only if using the bundled CMake setup; the headers themselves need no build step)

## Modules

All public names live in the `urlicht` namespace (with per-module inner namespaces such as `urlicht::container`).
The umbrella header `<urlicht/urlicht.h>` includes everything.

### algorithm

Range-friendly algorithms as direct replacements for their standard counterparts under std::ranges:

- `urlicht::algorithm::lower_bound` — a branchless binary search built on `std::bit_floor`/`std::bit_ceil`
  (the "Shar's algorithm" style), with iterator/sentinel and range overloads, projections, and `constexpr`
  support. 
- `urlicht::algorithm::upper_bound` — `lower_bound` with a swapped comparator.
- `urlicht::algorithm::binary_search` — `lower_bound` plus one membership comparison.

### any

- `urlicht::any::adaptive_any<OptimizeForSize, OptimizeForAlign>` — a `std::any` replacement whose
  small-buffer size and alignment are template parameters. Types that fit are stored inline; types with
  throwing moves are always heap-allocated so that `adaptive_any`'s own move is `noexcept`. Includes a
  static vtable (no per-type heap indirection), conditional construction, `emplace`, `swap`, `in_sbo()`,
  and the full `any_cast` family.
- `urlicht::any::any_view` — a lightweight, non-owning, type-erased view with cheap `is<T>()` checks and
  pointer-identity comparison.

### concepts

`<urlicht/concepts/concepts.h>` defines ~40 reusable concepts, including iterator/range concepts,
container concepts (`contiguous_container`, `reservable_container`, `unordered_map`, …), and many others 
— which are used throughout the library to harden template constraints.

### concurrency

- `mpmc_queue<T, Policy>` — a bounded multi-producer/multi-consumer queue in the Vyukov style with per-slot
  turn sequence numbers, monotonic indices (no modulo), cache-line-aligned cursors, and lambda-based in-place
  consumption. Requires only nothrow-destructible element types.
- `spsc_queue<T, Policy>` — a single-producer/single-consumer queue with cached indices (2 + 1/n atomic
  operations per pair of operations on average), batch pushes (`push_range`, `push_n`, `push_n_from`),
  and peek-style consumption (`try_apply_front`, `consume_all`).
- `static_sharded_counter<T, Threshold, NumLocalCnt>` — a sharded approximate counter with cache-line-padded
  per-thread slots, relaxed memory ordering, and `get_approximate()`/`get_exact()` accessors.

### container

- `urlicht::container::flat_map<Key, Mapped, KeyCompare, KeyContainer, MappedContainer, LowerBoundFn>` —
  a sorted SoA (parallel key/value arrays) associative container implementing the full `std::flat_map`
  interface, plus a template-configurable lower-bound strategy (defaulting to Urlicht's branchless search),
  `sorted`/`sorted_unique` constructor tags, and heterogeneous lookup.
- `urlicht::container::inplace_vector<T, N>` — a fixed-capacity, stack-allocated vector following
  `std::inplace_vector` (P0843), with an adaptive index type (8/16/32/64-bit, chosen by `N`) and fallible
  `try_*` operations.
- `urlicht::container::d_ary_heap<T, Container, Comp, HeapPolicy, StabilityCounter>` — a policy-driven d-ary heap with
  template-configurable *arity*, *stability* (insertion-order tie-breaking), and *mutability* (per-element handle for modification). 
  If the heap is mutable, optional id recycling and handle generation tracking are available. Zero-overhead is achieved
  across the 10+ orthogonal policies, and a `priority_queue` compatibility alias is provided.
- `urlicht::container::dense_disjoint_sets<Id, Policy>` — union-find over a dense `[0, n)` id space with
  compile-time union heuristics (`by_size`, `by_rank`) and path compression policies (`path_halving`,
  `full_compression`).

### functional

- `urlicht::functional::flexible_function<Ret(Args...) [cv/ref/noexcept], OptimizeForSize, OptimizeForAlign, Alloc>`
  and `flexible_move_only_function` — allocator-aware, type-erased callables covering all twelve
  cv/ref/noexcept signature variants. Compared to `std::function`, they offer customizable small-buffer
  size/alignment, specifier-aware types, and typed allocator/PMR support. Captureless functions can be
  stored via `urlicht::nontype<f>` with no storage at all.

### memory

- `urlicht::memory::arena<Opt, GrowthPolicy, UpstreamAlloc>` — a bump allocator that hands out memory
  backwards from the end of each buffer, keeps its chunk metadata in-buffer, and supports `reset()` (rewind)
  and `release()` (free all). Its `Opt` is a `urlicht::memory::resource_options` value,
  passed as a non-type template parameter; a zero-overhead fixed-buffer mode is available via
  `arena<{.use_upstream = false}>`.
- `urlicht::memory::concurrent_arena` — a mutex-synchronized arena for sharing across threads.
- `urlicht::memory::resource_view<T, Resource, Options>` — a non-owning, STL-compatible allocator that lets standard containers
  (`std::vector`, `std::unordered_map`, …) and Urlicht containers allocate from a shared memory resource (e.g. arena).
- `urlicht::memory::pmr::arena_resource<Arena, Options>` — adapts an arena to `std::pmr::memory_resource`.
- `urlicht::memory::observer_ptr<T>` — a non-owning smart pointer to a managed object, intended as a substitute to
  raw pointers.
- `urlicht::memory::tagged_ptr<T, IS_OWNING>` — squeezes tag bits into the unused bits of an aligned pointer,
  with an owning smart-pointer mode.
- `urlicht::memory::huge_pages` — RAII allocation of huge pages (uses mmap on Linux/macOS, VirtualAlloc2 on Windows).

### internal

Implementation plumbing (`config.h` portability macros, dispatch tags such as `sorted`, `heapified`, and
`nontype`, a `scope_guard`, allocator traits helpers). Not intended for direct use.

## Usage

Urlicht is header-only. After making `include/` available (see [Building and installing](#building-and-installing)),
link against `Urlicht::urlicht` or simply add `include/` to your include path.

A mutable heap for the Dijkstra algorithm:

```cpp
#include <urlicht/container/d_ary_heap.h>

struct heap_info {
    size_t vertex;
    double dist;
};

using heap_t = urlicht::container::d_ary_heap<
    heap_info, 
    std::vector, 
    std::greater<>, 
    {.mutable_ = true, .reuse_id = true, .track_generation = false}
>;

heap_t heap; heap.reserve(n);

std::vector<heap_t::handle_type> handles(graph.size());
handles[SRC_IDX] = heap.push({SRC_IDX, 0.0});

while(!heap.empty()) {
    // Perform the algorithm
}
```

A bounded lock-free MPMC task queue with compile-time capacity:

```cpp
#include <urlicht/concurrency/mpmc_queue.h>

urlicht::concurrency::mpmc_queue<int, urlicht::concurrency::capacity<1024>> queue;

// producer side
for(...) {
  queue.emplace(val);
}                 

// consumer side
std::vector<int> received;
received.reserve(n);
while(...) {
  queue.consume_front([&](auto&& val) noexcept {  // No extra copies!
    received.emplace_back(std::move(val));
  });
}
```

A type-erased callable with a custom small-buffer size:

```cpp
#include <urlicht/functional/flexible_move_only_function.h>

urlicht::functional::flexible_move_only_function<void(int), /* SBO size in bytes = */ 128> task =
    [handle = get_handle()](int id) { handle.run(id); };
```

Allocating standard containers from a shared arena using huge pages:

```cpp
#include <urlicht/memory/arena.h>
#include <urlicht/memory/resource_view.h>
#include <urlicht/memory/huge_pages.h>

urlicht::memory::huge_pages huge_pages(urlicht::memory::huge_page_size::SIZE_2MB, /* page count = */ 10);
urlicht::memory::arena<> arena(huge_pages.get(), /* buffer size = */ 2 * 1024 * 1024 * 10);
urlicht::memory::resource_view<int> view(arena);
std::vector<int, urlicht::memory::resource_view<int>> scratch(view);
// ... arena.reset() reclaims everything at once
```

Complete, runnable demonstrations of every component live in [`examples/`](examples/).

## Building and installing

```sh
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --install build   # installs headers and the CMake target
```

Available options (all default to `OFF`):

| Option                     | Default | Description              |
|----------------------------|---------|--------------------------|
| `URLICHT_BUILD_TESTS`      | `OFF`   | Build the unit tests     |
| `URLICHT_BUILD_BENCHMARKS` | `OFF`   | Build the benchmarks     |
| `URLICHT_BUILD_EXAMPLES`   | `OFF`   | Build the example programs |

To consume the library from another CMake project:

```cmake
include(FetchContent)
FetchContent_Declare(urlicht
    GIT_REPOSITORY https://github.com/wwwongad/urlicht.git
    GIT_TAG main)
FetchContent_MakeAvailable(urlicht)

target_link_libraries(my_target PRIVATE Urlicht::urlicht)
```

## Testing, benchmarks, and examples

- **Tests** use [GoogleTest](https://github.com/google/googletest) (fetched automatically if not installed).
  Build and run with `ctest --test-dir build` after configuring with the default options.
- **Benchmarks** use [Google Benchmark](https://github.com/google/benchmark) (also fetched automatically if not installed) and compare Urlicht's components
  against the standard library, and — when
  found — the Boost library. Configure with
  `-DURLICHT_BUILD_BENCHMARKS=ON`.
- **Examples** — one small program per public component under [`examples/`](examples/). Configure with
  `-DURLICHT_BUILD_EXAMPLES=ON`.

## License

Distributed under the [MIT License](LICENSE).
