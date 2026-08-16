#ifndef URLICHT_URLICHT_H
#define URLICHT_URLICHT_H

#include <urlicht/algorithm/binary_search.h>
#include <urlicht/algorithm/lower_bound.h>
#include <urlicht/algorithm/upper_bound.h>

#include <urlicht/any/adaptive_any.h>
#include <urlicht/any/any_view.h>

#include <urlicht/concepts/concepts.h>

#include <urlicht/concurrency/mpmc_queue.h>
#include <urlicht/concurrency/spsc_queue.h>
#include <urlicht/concurrency/static_sharded_counter.h>

#include <urlicht/container/d_ary_heap.h>
#include <urlicht/container/dense_disjoint_sets.h>
#include <urlicht/container/flat_map.h>
#include <urlicht/container/inplace_vector.h>

#include <urlicht/functional/flexible_function.h>
#include <urlicht/functional/flexible_move_only_function.h>

#include <urlicht/memory/arena.h>
#include <urlicht/memory/arena_view.h>
#include <urlicht/memory/concurrent_arena.h>
#include <urlicht/memory/pmr/arena_resource.h>
#include <urlicht/memory/tagged_ptr.h>

#endif // URLICHT_URLICHT_H
