#include <urlicht/concurrency/mpmc_queue.h>
#include <gtest/gtest.h>
#include <urlicht/memory/arena_view.h>
#include <urlicht/memory/pmr/arena_resource.h>
#include <barrier>
#include <thread>
#include <random>
#include <algorithm>
#include <mutex>
#include <vector>
#include "utils/tracked_int.h"

using namespace urlicht::concurrency;

static_assert(urlicht::is_urlicht_mpmc_queue_v<mpmc_queue<int, capacity<1024>>>);
static_assert(urlicht::is_urlicht_mpmc_queue_v<pmr::mpmc_queue<std::string>>);

TEST(MPMCQueue, Initialization) {
    auto check_empty = [](auto& q) {
        EXPECT_EQ(q.capacity(), 1024);
        EXPECT_TRUE(q.empty());
        EXPECT_FALSE(q.full());
        EXPECT_EQ(q.size(), 0);
    };

    mpmc_queue<int, capacity<1024>> fixed;
    check_empty(fixed);
    mpmc_queue<int> runtime{1024};
    check_empty(runtime);

    EXPECT_THROW((mpmc_queue<int>{1000}), std::invalid_argument);
    EXPECT_THROW((mpmc_queue<int>{1}), std::invalid_argument);  // N must be greater than 1
    EXPECT_THROW((mpmc_queue<int>{0}), std::invalid_argument);
}

TEST(MPMCQueue, StatefulAlloc) {
    {
        urlicht::memory::arena<> arena{1 << 22}; // 4MB
        const urlicht::memory::arena_view<int> alloc{arena};
        mpmc_queue<int, urlicht::memory::arena_view<int>> q{1024, alloc};

        static_assert(std::uses_allocator_v<decltype(q), urlicht::memory::arena_view<int>>);
        EXPECT_EQ(q.capacity(), 1024);
        const auto& buf = arena.get_initial_buffer();
        EXPECT_GE(buf.end() - buf.curr, 1024 * sizeof(detail::mpmc_slot_<int>));
    } {
        urlicht::memory::pmr::arena_resource<> res{1 << 22};
        pmr::mpmc_queue<std::string> q{256, &res};

        static_assert(
            std::uses_allocator_v<decltype(q), std::pmr::polymorphic_allocator<std::string>>
        );
        EXPECT_EQ(q.capacity(), 256);
        const auto& buf = res.arena().get_initial_buffer();
        EXPECT_GE(buf.end() - buf.curr, 256 * sizeof(detail::mpmc_slot_<std::string>));
    }
}

TEST(MPMCQueue, BasicReadWrite) {
    {
        mpmc_queue<tracked_int> q{16};

        EXPECT_TRUE(q.try_emplace(1));
        EXPECT_TRUE(q.try_emplace(2));
        EXPECT_TRUE(q.try_emplace(3));

        EXPECT_FALSE(q.empty());
        EXPECT_EQ(q.size(), 3);

        tracked_int first;
        EXPECT_TRUE(q.try_dequeue(first));
        EXPECT_EQ(first.value(), 1);

        const auto second = q.try_dequeue();
        ASSERT_TRUE(second.has_value());
        EXPECT_EQ(second->value(), 2);

        tracked_int third = q.dequeue();
        EXPECT_EQ(third.value(), 3);

        EXPECT_TRUE(q.empty());
        EXPECT_EQ(q.size(), 0);
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(MPMCQueue, OnFullAndEmpty) {
    mpmc_queue<int> q{4};

    for (int i = 0; i < 4; ++i) {
        q.push(i);
    }

    EXPECT_TRUE(q.full());
    EXPECT_FALSE(q.empty());
    EXPECT_FALSE(q.try_emplace(4));

    for (int i = 0; i < 4; ++i) {
        const auto value = q.try_dequeue();
        ASSERT_TRUE(value.has_value());
        EXPECT_EQ(*value, i);
    }

    EXPECT_TRUE(q.empty());
    EXPECT_FALSE(q.full());
    EXPECT_FALSE(q.try_dequeue().has_value());

    int unchanged = 1234;
    EXPECT_FALSE(q.try_dequeue(unchanged));
    EXPECT_EQ(unchanged, 1234);
}

TEST(MPMCQueue, Consume) {
    {
        mpmc_queue<tracked_int> q{4};
        q.emplace(1);
        q.emplace(2);

        int val{};
        EXPECT_TRUE(q.try_consume_front([&val](auto&& front) noexcept {
            val = front.value();
        }));
        EXPECT_EQ(val, 1);
        EXPECT_EQ(q.size(), 1);

        q.consume_front([&val](auto&& front) noexcept {
            val = front.value() * 10;
        });
        EXPECT_EQ(val, 20);
        EXPECT_TRUE(q.empty());

        EXPECT_FALSE(q.try_consume_front([](auto&&) noexcept {}));
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(MPMCQueue, SlotsRecycling) {
    constexpr std::size_t capacity = 4;
    constexpr std::size_t rounds = 10'000;

    mpmc_queue<tracked_int> q{capacity};

    for (std::size_t round = 0; round < rounds; ++round) {
        for (std::size_t i = 0; i < capacity; ++i) {
            q.push(round * capacity + i);
        }

        EXPECT_TRUE(q.full());
        EXPECT_FALSE(q.try_emplace(1234));

        for (std::size_t i = 0; i < capacity; ++i) {
            EXPECT_EQ(q.dequeue().value(), round * capacity + i);
        }
        EXPECT_TRUE(q.empty());
    }
    EXPECT_TRUE(check_lifetime());
}

TEST(MPMCQueue, ClearAndReuse) {
    {
        mpmc_queue<tracked_int> q{4};
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

TEST(MPMCQueue, ConcurrentStress) {
    constexpr std::size_t items_per_producer = 50'000;
    constexpr std::size_t repeats = 10;

    const auto run_stress = [](const std::size_t producer_count, const std::size_t consumer_count) {
        ASSERT_EQ(
            items_per_producer * producer_count % consumer_count,
            0U
        );

        const std::size_t total = items_per_producer * producer_count;
        const std::size_t items_per_consumer = total / consumer_count;

        for (std::size_t repeat = 0; repeat < repeats; ++repeat) {
            mpmc_queue<tracked_int> q{1024};
            std::barrier sync(static_cast<std::ptrdiff_t>(producer_count + consumer_count));
            std::vector<unsigned char> consumed(total);
            std::mutex consumed_mutex;

            bool value_out_of_range = false;
            bool values_consumed_once = true;  // additional guard in case {consumed} wraps around

            const auto record = [&](const tracked_int& value) {
                const auto index = static_cast<std::size_t>(value.value());
                std::lock_guard lock(consumed_mutex);
                if (index >= total) {
                    value_out_of_range = true;
                    return;
                }
                ++consumed[index];
                values_consumed_once &= (consumed[index] == 1);
            };

            std::vector<std::thread> producers;
            producers.reserve(producer_count);
            for (std::size_t producer = 0; producer < producer_count; ++producer) {
                producers.emplace_back([&, producer] {
                    std::mt19937 rng(0xC0FFEE);
                    std::size_t next = producer * items_per_producer;
                    const std::size_t end = next + items_per_producer;

                    sync.arrive_and_wait();
                    while (next != end) {
                        if (rng() % 2 == 0) {
                            if (q.try_emplace(static_cast<int>(next))) {
                                ++next;
                            }
                        } else {
                            q.emplace(static_cast<int>(next));
                            ++next;
                        }
                    }
                });
            }

            std::vector<std::thread> consumers;
            consumers.reserve(consumer_count);
            for (std::size_t consumer = 0; consumer < consumer_count; ++consumer) {
                consumers.emplace_back([&] {
                    std::mt19937 rng(0xBADF00D);
                    std::size_t consumed_count = 0;

                    sync.arrive_and_wait();
                    while (consumed_count < items_per_consumer) {
                        switch (rng() % 6) {
                            case 0: {
                                if (tracked_int value; q.try_dequeue(value)) {
                                    record(value);
                                    ++consumed_count;
                                }
                                break;
                            }
                            case 1: {
                                if (auto value = q.try_dequeue()) {
                                    record(*value);
                                    ++consumed_count;
                                }
                                break;
                            }
                            case 2: {
                                if (q.try_consume_front([&record](tracked_int&& value) noexcept {
                                    record(value);
                                })) {
                                    ++consumed_count;
                                }
                                break;
                            }
                            case 3: {
                                tracked_int value;
                                q.dequeue(value);
                                record(value);
                                ++consumed_count;
                                break;
                            }
                            case 4: {
                                const auto value = q.dequeue();
                                record(value);
                                ++consumed_count;
                                break;
                            }
                            default: {
                                q.consume_front([&record](tracked_int&& value) noexcept {
                                    record(value);
                                });
                                ++consumed_count;
                                break;
                            }
                        }
                    }
                });
            }

            for (auto& producer : producers) {
                producer.join();
            }
            for (auto& consumer : consumers) {
                consumer.join();
            }

            EXPECT_FALSE(value_out_of_range);
            EXPECT_TRUE(values_consumed_once);
            EXPECT_TRUE(q.empty());
            EXPECT_EQ(q.size(), 0);
            EXPECT_TRUE(std::ranges::all_of(
                consumed,
                [](const unsigned char value) { return value == 1; }
            ));
        }
    };

    run_stress(1, 8);
    run_stress(8, 1);
    run_stress(8, 8);
    EXPECT_TRUE(check_lifetime());
}