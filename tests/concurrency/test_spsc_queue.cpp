#include <urlicht/concurrency/spsc_queue.h>
#include <gtest/gtest.h>
#include <urlicht/memory/arena_view.h>
#include <urlicht/memory/pmr/arena_resource.h>
#include <barrier>
#include <thread>
#include <random>
#include "utils/tracked_int.h"


using namespace urlicht::concurrency;

static_assert(urlicht::is_urlicht_spsc_queue_v<spsc_queue<int, capacity<1024>>>);
static_assert(urlicht::is_urlicht_spsc_queue_v<pmr::spsc_queue<std::string>>);

TEST(SPSCQueue, Initialization) {
    auto check_empty = [](auto& q) {
        EXPECT_EQ(q.capacity(), 1024);
        EXPECT_TRUE(q.empty());
        EXPECT_FALSE(q.full());
        EXPECT_EQ(q.size(), 0U);
    };

    spsc_queue<int, capacity<1024>> q1;
    check_empty(q1);
    spsc_queue<int> q2{1024};
    check_empty(q2);

    EXPECT_THROW(spsc_queue<int> q3{1000}, std::invalid_argument); // Not power-of-2
    EXPECT_THROW(spsc_queue<int> q3{0}, std::invalid_argument);
}

TEST(SPSCQueue, StatefulAllocator) {
    {
        urlicht::memory::arena<> arena{1 << 14};
        const urlicht::memory::arena_view<int> alloc{arena};
        spsc_queue<int, urlicht::memory::arena_view<int>> q{1024, alloc};

        static_assert(std::uses_allocator_v<decltype(q), urlicht::memory::arena_view<int>>);
        EXPECT_EQ(q.capacity(), 1024);
        const auto& buf = arena.get_initial_buffer();
        EXPECT_GE(buf.end() - buf.curr, 1024 * sizeof(int));
    } {
        urlicht::memory::pmr::arena_resource<> res{1 << 14};
        pmr::spsc_queue<std::string> q{256, &res};

        static_assert(
            std::uses_allocator_v<decltype(q), std::pmr::polymorphic_allocator<std::string>>
        );
        EXPECT_EQ(q.capacity(), 256);
        const auto& buf = res.arena().get_initial_buffer();
        EXPECT_GE(buf.end() - buf.curr, 256 * sizeof(std::string));
    }
}

TEST(SPSCQueue, BasicReadWrite) {
    {
        spsc_queue<tracked_int> q{16};
        q.try_emplace(1);
        q.try_emplace(2);
        q.try_emplace(3);

        EXPECT_FALSE(q.empty());
        EXPECT_EQ(q.size(), 3);

        tracked_int x;
        EXPECT_TRUE(q.try_dequeue(x));
        EXPECT_EQ(x.value(), 1);

        const auto opt = q.try_dequeue();
        EXPECT_EQ(opt.value().value(), 2);

        tracked_int y = q.dequeue();
        EXPECT_EQ(y.value(), 3);

        EXPECT_TRUE(q.empty());
        EXPECT_EQ(q.size(), 0U);
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(SPSCQueue, OnFullAndEmpty) {
    spsc_queue<int> q{4};
    for (int i = 0; i < 4; ++i) {
        q.push(i);
    }
    EXPECT_TRUE(q.full());
    EXPECT_FALSE(q.try_emplace(0));

    for (int i = 0; i < 4; ++i) {
        EXPECT_EQ(q.try_dequeue().value(), i);
    }
    EXPECT_TRUE(q.empty());

    EXPECT_FALSE(q.try_dequeue().has_value());
    int t = 1234;
    EXPECT_FALSE(q.try_dequeue(t));
    EXPECT_EQ(t, 1234); // Unchanged
}

TEST(SPSCQueue, ApplyAndConsume) {
    {
        spsc_queue<tracked_int> q{4};
        q.emplace(1);
        q.emplace(2);

        int val{};
        EXPECT_TRUE(q.try_consume_front([&val](auto&& front) noexcept {
            val = front.value();
        }));
        EXPECT_EQ(val, 1);
        EXPECT_EQ(q.size(), 1);

        EXPECT_TRUE(q.try_apply_front([&val](auto&& front) noexcept {
            val = front.value();
        }));
        EXPECT_EQ(val, 2);
        // Front elem not consumed
        EXPECT_EQ(q.size(), 1);
        EXPECT_EQ(q.try_dequeue().value().value(), 2);
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(SPSCQueue, PushRangeAndDequeueAll) {
    {
        auto add_items = [](auto& vec, const std::size_t n) {
            for (std::size_t i = 0; i < n; ++i) vec.push_back(i);
        };

        std::vector<tracked_int> vec; add_items(vec, 5);
        spsc_queue<tracked_int> q{8};

        auto push_and_check = [&](const std::size_t exp) {
#if !UL_HAS_CPP23
            const auto s = q.push_range(
                std::ranges::subrange{std::make_move_iterator(vec.begin()), std::make_move_iterator(vec.end())});
            EXPECT_EQ(s, exp);
#else
            EXPECT_EQ(q.push_range(vec | std::views::as_rvalue), exp);
#endif
        };
        push_and_check(5);
        vec.clear(); add_items(vec, 5);
        push_and_check(3);  // q = {0, 1, 2, 3, 4, 0, 1, 2}

        // Pop 5 to create a wrap-around
        for (int i = 0; i < 5; ++i) {
            EXPECT_EQ(q.dequeue().value(), i);
        }
        vec.clear(); add_items(vec, 3);
        push_and_check(3);  // q = {0, 1, 2, 0, 1, 2}

        std::vector<tracked_int> res;
        EXPECT_EQ(q.dequeue_all(std::back_inserter(res)), 6);
        for (int i = 0; auto& x : res) {
            EXPECT_EQ(x.value(), i++ % 3);
        }
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(SPSCQueue, PushN) {
    spsc_queue<int> q{8};
    EXPECT_EQ(q.push_n(6, 111), 6);
    EXPECT_EQ(q.push_n(4, 234), 2);

    for (int i = 0; i < 4; ++i) {
        EXPECT_EQ(q.dequeue(), 111);
    }
    EXPECT_EQ(q.push_n(10, 888), 4);
    for (int i = 0; i < 2; ++i) {
        EXPECT_EQ(q.try_dequeue().value(), 111);
    }
    EXPECT_EQ(q.push_n(3, 100), 2);

    std::vector<int> res;
    EXPECT_EQ(q.dequeue_all(std::back_inserter(res)), 8);
    EXPECT_EQ(res, (std::vector{234, 234, 888, 888, 888, 888, 100, 100 }));
}

TEST(SPSCQueue, PushWithGenerator) {
    {
        int num = 0;
        auto gen = [&num] { return tracked_int{num++}; };
        spsc_queue<tracked_int> q{8};
        EXPECT_EQ(q.push_n_from(12, gen), 8);

        for (int i = 0; i < 6; ++i) {
            EXPECT_EQ(q.dequeue().value(), i);
        }

        EXPECT_EQ(q.push_n_from(20, gen), 6); // gen should start from 8

        int exp = 6;
        const auto cnt = q.consume_all([&exp](auto&& val) noexcept {
            EXPECT_EQ(val.value(), exp++);
        });
        EXPECT_EQ(cnt, 8);
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(SPSCQueue, ClearAndReuse) {
    {
        spsc_queue<tracked_int> q{4};
        q.emplace(1);
        q.emplace(2);
        EXPECT_FALSE(q.empty());
        EXPECT_EQ(q.size(), 2);

        q.clear();
        EXPECT_TRUE(q.empty());
        EXPECT_EQ(q.size(), 0U);

        EXPECT_TRUE(q.try_emplace(3));
        EXPECT_EQ(q.try_dequeue().value().value(), 3);
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(SPSCQueue, ConcurrentStress) {
    {
        constexpr std::size_t total = 20'000'000;
        spsc_queue<tracked_int> q{1024};

        std::barrier sync(2);

        // Producer
        std::thread producer([&] {
            std::mt19937 rng(0xC0FFEE);
            std::size_t next = 0;

            sync.arrive_and_wait();
            while (next < total) {
                switch (rng() % 3) {
                    case 0: {
                        if (q.try_emplace(static_cast<int>(next)))
                            ++next;
                        break;
                    }
                    case 1: { // [guaranteed] push
                        q.emplace(static_cast<int>(next));
                        ++next;
                        break;
                    }
                    default: {
                        const std::size_t k =
                            std::min<std::size_t>(1 + rng() % 16, total - next);
                        auto gen = [&] { return next++; };
                        q.push_n_from(k, gen);
                        break;
                    }
                }
            }
        });

        // Consumer (this thread)
        std::mt19937 rng(0xBADF00D);
        bool order_ok = true;
        std::size_t expected = 0;

        auto verify = [&](const tracked_int& x) {
            if (x.value() != static_cast<int>(expected)) {
                order_ok = false;
            }
            ++expected;
        };

        sync.arrive_and_wait();
        while (expected < total && order_ok) {
            switch (rng() % 3) {
                case 0: { // try_dequeue
                    if (tracked_int x; q.try_dequeue(x)) {
                        verify(x);
                    }
                    break;
                }
                case 1: { // [guaranteed] dequeue
                    verify(q.dequeue());
                    break;
                }
                default: {
                    std::vector<tracked_int> out;
                    q.dequeue_all(std::back_inserter(out));
                    for (const auto& v : out) {
                        verify(v);
                    }
                    break;
                }
            }
        }

        producer.join();
        EXPECT_TRUE(order_ok);
        EXPECT_TRUE(q.empty());
        EXPECT_EQ(q.size(), 0U);
    }
    EXPECT_TRUE(check_lifetime());
}

// Dequeue operations are required to be noexcept; single push operations trivially have strong
// exception safety. We only need to test batch insertion methods to see whether they destroy
// all elements constructed upon an exception.
TEST(SPSCQueue, ExceptionSafety) {
    {
        int cnt = 0;
        auto gen = [&] {
            if (cnt == 3) {
                cnt = 0;
                throw std::bad_alloc{};
            }
            return tracked_int{cnt++};
        };
        spsc_queue<tracked_int> q{8};
        q.try_emplace(1); q.try_emplace(2);

        EXPECT_THROW(q.push_n_from(5, gen), std::bad_alloc);
        // Unchanged
        EXPECT_EQ(q.size(), 2);
        EXPECT_EQ(q.try_dequeue().value().value(), 1);
        EXPECT_EQ(q.try_dequeue().value().value(), 2);
    }
    EXPECT_TRUE(check_lifetime());
}