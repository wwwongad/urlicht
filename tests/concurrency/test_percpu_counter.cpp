#include <urlicht/concurrency/percpu_counter.h>
#include <gtest/gtest.h>
#include <atomic>
#include <cstdint>
#include <thread>
#include <vector>
#include <numeric>
#include <random>
#include <algorithm>
#include <barrier>


template <typename F>
void run_threads(std::size_t n, F&& fn) {
    std::vector<std::thread> ts;
    ts.reserve(n);
    for (std::size_t i = 0; i < n; ++i) {
        ts.emplace_back([&, i] { fn(i); });
    }
    for (auto& t : ts) t.join();
}


TEST(PerCpuCounter, StaticMethods) {
    using C = urlicht::percpu_counter<std::int64_t, 17, 9>;
    static_assert(C::threshold() == 17);
    static_assert(C::num_local_cnt() == 9);
    SUCCEED();
}

TEST(PerCpuCounter, DefaultInit) {
    urlicht::percpu_counter<std::int64_t, 8, 2> c;
    EXPECT_EQ(c.get_exact(), 0);
    EXPECT_EQ(c.get_approximate(), 0);
}

TEST(PerCpuCounter, InitWithValue) {
    urlicht::percpu_counter<std::int64_t, 8, 2> c(123);
    EXPECT_EQ(c.get_approximate(), 123);
    EXPECT_EQ(c.get_exact(), 123);
}

TEST(PerCpuCounter, IncrementLocal) {
    using C = urlicht::percpu_counter<std::int64_t, 1000, 1>;
    C c;
    c.increment();
    c.increment(4);
    EXPECT_EQ(c.get_approximate(), 0);
    EXPECT_EQ(c.get_exact(), 5);
    EXPECT_EQ(c.get_approximate(), 5);
}

TEST(PerCpuCounter, FlushOnThresholdUnsigned) {
    urlicht::percpu_counter<std::uint64_t, 4, 1> c;
    c.increment();
    c.increment();
    c.increment();
    EXPECT_EQ(c.get_approximate(), 0u);
    c.increment(); //4 -> flush
    EXPECT_EQ(c.get_approximate(), 4u);
    EXPECT_EQ(c.get_exact(), 4u);
}

TEST(PerCpuCounter, FlushOnNegativeThresholdSigned) {
    using C = urlicht::percpu_counter<std::int64_t, 3, 1>;
    C c;
    c.increment(-1);
    c.increment(-1);
    EXPECT_EQ(c.get_approximate(), 0);
    c.increment(-1); // -3 -> flush
    EXPECT_EQ(c.get_approximate(), -3);
    EXPECT_EQ(c.get_exact(), -3);
}

TEST(PerCpuCounter, FlushLocal) {
    urlicht::percpu_counter<std::int64_t, 1000, 1> c;
    c.increment(7);
    EXPECT_EQ(c.get_approximate(), 0);
    c.flush_local();
    EXPECT_EQ(c.get_approximate(), 7);
    EXPECT_EQ(c.get_exact(), 7);
}

TEST(PerCpuCounter, FlushAll) {
    urlicht::percpu_counter<std::int64_t, 1000, 4> c;
    constexpr std::size_t threads = 8;
    constexpr int per_thread = 1025;

    run_threads(threads, [&](std::size_t) {
        for (int i = 0; i < per_thread; ++i) c.increment();
    });

    const auto not_flushed = c.get_approximate();
    EXPECT_LT(not_flushed, static_cast<std::int64_t>(threads * per_thread));

    const auto flushed = c.get_exact();
    EXPECT_EQ(flushed, static_cast<std::int64_t>(threads * per_thread));
    EXPECT_EQ(c.get_approximate(), flushed);
}


TEST(PerCpuCounter, PredicateFalse) {
    urlicht::percpu_counter<std::int64_t, 1000, 1> c;
    c.increment(10);
    c.flush_if([](std::int64_t x) { return x >= 100; });
    EXPECT_EQ(c.get_approximate(), 0);
    EXPECT_EQ(c.get_exact(), 10);
}

TEST(PerCpuCounter, PredicateTrue) {
    urlicht::percpu_counter<std::int64_t, 1000, 1> c;
    c.increment(10);
    c.flush_if([](std::int64_t x) { return x >= 10; });
    EXPECT_EQ(c.get_approximate(), 10);
    EXPECT_EQ(c.get_exact(), 10);
}

TEST(PerCpuCounter, SetNewValue) {
    urlicht::percpu_counter<std::int64_t, 1000, 1> c;
    c.increment(999);
    EXPECT_EQ(c.get_approximate(), 0);
    c.set(42);
    EXPECT_EQ(c.get_approximate(), 42);
    EXPECT_EQ(c.get_exact(), 42);
    c.increment(20);  // Original local value is cleared
    EXPECT_EQ(c.get_approximate(), 42);
}

TEST(PerCpuCounter, ConstGetExactNoFlush) {
    using C = urlicht::percpu_counter<std::int64_t, 1000, 1>;
    C c;
    c.increment(15);
    const C& cc = c;

    EXPECT_EQ(cc.get_exact(), 15); // sums global + locals
    EXPECT_EQ(cc.get_approximate(), 0); // global only
}

TEST(PerCpuCounter, HighlyMultithreaded) {
    urlicht::percpu_counter<std::int64_t, 256, 8> c;

    constexpr std::size_t threads = 1024u;
    constexpr std::int64_t per_thread = 200000;

    run_threads(threads, [&](std::size_t) {
        for (std::int64_t i = 0; i < per_thread; ++i) c.increment();
    });

    EXPECT_EQ(c.get_exact(), static_cast<std::int64_t>(threads) * per_thread);
}

TEST(PerCpuCounter, MixedSignedDiffs) {
    urlicht::percpu_counter<std::int64_t, 64, 8> c;

    constexpr std::size_t threads = 1024u;
    constexpr std::int64_t iters = 100000;

    run_threads(threads, [&](std::size_t tid) {
        const std::int64_t sign = (tid % 2 == 0) ? 1 : -1;
        for (std::int64_t i = 0; i < iters; ++i) {
            c.increment(sign);
            if ((i & 7) == 0) c.increment(-sign);
        }
    });

    constexpr auto expected = 0u; // Hardcoded
    EXPECT_EQ(c.get_exact(), expected);
}

TEST(PerCpuCounter, ConcurrentReadersAndWriters) {
    using C = urlicht::percpu_counter<std::int64_t, 128, 8>;
    C c;

    constexpr std::size_t writers = 256u;
    constexpr std::size_t readers = 256u;
    constexpr std::int64_t per_writer = 150000;

    std::barrier sync_point{writers + readers};

    std::atomic done{false};
    std::vector<std::thread> ts;
    ts.reserve(writers + readers);

    for (std::size_t i = 0; i < writers; ++i) {
        ts.emplace_back([&] {
            sync_point.arrive_and_wait();
            for (std::int64_t k = 0; k < per_writer; ++k) {
                c.increment();
            }
        });
    }

    for (std::size_t i = 0; i < readers; ++i) {
        ts.emplace_back([&] {
            sync_point.arrive_and_wait();
            while (!done.load(std::memory_order_acquire)) {
                c.get_approximate();
                static_cast<const C&>(c).get_exact();
            }
        });
    }

    for (std::size_t i = 0; i < writers; ++i) {
        ts[i].join();
    }
    done.store(true, std::memory_order_release);
    for (std::size_t i = writers; i < ts.size(); ++i) {
        ts[i].join();
    }
    EXPECT_EQ(c.get_exact(), static_cast<std::int64_t>(writers) * per_writer);
}
