#include <benchmark/benchmark.h>
#include <urlicht/internal/config.h>
#include <urlicht/concurrency/spsc_queue.h>
#include "third_party/rigtorp_spsc.h"
#include <queue>
#include <mutex>
#include <barrier>
#include <memory>
#include <condition_variable>

#if URLICHT_BM_HAS_BOOST
#include <boost/lockfree/spsc_queue.hpp>
#endif

#if UL_PLATFORM_LINUX
#include <pthread.h>
#endif

constexpr std::size_t STREAMING_SIZE = 1 << 20;
constexpr std::size_t BATCH_SIZE = 1 << 6;
constexpr std::size_t queue_size = 1 << 10;  // 1024
constexpr unsigned PRODUCER_CPU = 0;
constexpr unsigned CONSUMER_CPU = 1;

inline void pin_current_thread(unsigned cpu) {
#if UL_PLATFORM_LINUX
    cpu_set_t cpu_set;
    CPU_ZERO(&cpu_set);
    CPU_SET(cpu, &cpu_set);

    const int result = pthread_setaffinity_np(
        pthread_self(),
        sizeof(cpu_set_t),
        &cpu_set
    );
    if (result != 0) {
        throw std::system_error {
            result, std::generic_category(), "pthread_setaffinity_np failed"
        };
    }
#endif
}

using value_type1 = int;

struct info_t {
    std::uint64_t info_id;
    std::uint64_t user_id;
    double value;
    std::uint64_t quantity;
    std::uint64_t timestamp;
    bool side;
    std::uint8_t type;
    std::uint8_t status;
};
using value_type2 = info_t;

template <typename T>
auto make_value(const std::size_t id) {
    if constexpr (std::same_as<T, value_type1>) {
        return static_cast<value_type1>(id);
    } else if constexpr (std::same_as<T, value_type2>) {
        return info_t {
            .info_id = id
        };
    } else {
        throw std::logic_error {"invalid type"};
    }
}

template <typename T>
auto get_key(const T& value) noexcept {
    if constexpr (std::same_as<T, value_type1>) {
        return static_cast<std::uint64_t>(value);
    } else if constexpr (std::same_as<T, value_type2>) {
        return value.value;
    } else {
        throw std::logic_error {"invalid type"};
    }
}

template <typename T, std::size_t>
class mutex_queue {
    std::queue<T> queue_;
    std::size_t capacity_;
    std::mutex mutex_;
    std::condition_variable full_, empty_;
public:
    mutex_queue(const std::size_t size) : capacity_(size) {}

    void emplace(T&& val) noexcept {
        std::unique_lock lock(mutex_);
        full_.wait(lock, [this] { return queue_.size() < capacity_; });
        queue_.emplace(std::move(val));
        empty_.notify_one();
    }

    void dequeue(T& val) {
        std::unique_lock lock(mutex_);
        empty_.wait(lock, [this] { return !queue_.empty(); });
        val = std::move(queue_.front());
        queue_.pop();
        full_.notify_one();
    }
};

template <typename T, std::size_t>
class rigtorp_spsc_queue {
    rigtorp::SPSCQueue<T> queue_;
public:
    rigtorp_spsc_queue(const std::size_t size) : queue_{size} {}

    void emplace(T&& val) noexcept {
        queue_.emplace(std::move(val));
    }

    void dequeue(T& val) noexcept {
        while (!queue_.front()) {}
        val = std::move(*(queue_.front()));
        queue_.pop();
    }
};

#if URLICHT_BM_HAS_BOOST
template <typename T, std::size_t N>
class boost_spsc_queue {
    boost::lockfree::spsc_queue<T, boost::lockfree::capacity<N>> queue_;
public:
    boost_spsc_queue(const std::size_t) {}

    void emplace(T&& val) noexcept { while (!queue_.push(std::move(val))) {} }
    void dequeue(T& val) noexcept { while (!queue_.pop(val)) {} }

    template <typename Iter, typename Sent>
    std::size_t push_range(Iter begin, Sent end) {
        return queue_.push(begin, end) - begin;
    }

    template <typename Func>
    std::size_t consume_all(Func&& func) {
        return queue_.consume_all(std::forward<Func>(func));
    }
};
#endif

template <typename T, std::size_t N>
class urlicht_spsc_queue {
    urlicht::concurrency::spsc_queue<T, urlicht::concurrency::capacity<N>> queue_;
public:
    urlicht_spsc_queue(const std::size_t) {}

    void emplace(T&& val) noexcept { queue_.emplace(std::move(val)); }
    void dequeue(T& val) noexcept { queue_.dequeue(val); }

    template <typename Iter, typename Sent>
    std::size_t push_range(Iter begin, Sent end) {
        return queue_.push_range(std::ranges::subrange{begin, end});
    }

    template <typename Func>
    std::size_t consume_all(Func&& func) noexcept {
        return queue_.consume_all(std::forward<Func>(func));
    }
};

template <template <typename, std::size_t> typename Queue, typename T, std::size_t Size>
void BM_spsc_throughput(benchmark::State& st) {
    pin_current_thread(CONSUMER_CPU);

    for (auto _ : st) {
        st.PauseTiming();
        Queue<T, Size> queue{queue_size};
        std::barrier barrier{2};

        std::thread producer{[&] {
            pin_current_thread(PRODUCER_CPU);
            barrier.arrive_and_wait();

            for (std::size_t i = 0U; i < STREAMING_SIZE; ++i) {
                auto value = make_value<T>(i);
                queue.emplace(std::move(value));  // Guaranteed push
            }
        }};

        st.ResumeTiming();
        barrier.arrive_and_wait();
        // Consumer (main thread)
        for (std::size_t i = 0U; i < STREAMING_SIZE; ++i) {
            T top_value;
            queue.dequeue(top_value);
            benchmark::DoNotOptimize(top_value);
        }

        producer.join();
        benchmark::ClobberMemory();
    }

    st.SetItemsProcessed(st.iterations() * STREAMING_SIZE);
    st.SetBytesProcessed(st.iterations() * STREAMING_SIZE * sizeof(T));
}


template <template <typename, std::size_t> typename Queue, typename T, std::size_t Size>
void BM_spsc_bulk_throughput(benchmark::State& st) {
    constexpr std::size_t iterations = STREAMING_SIZE / BATCH_SIZE;
    pin_current_thread(CONSUMER_CPU);

    for (auto _ : st) {
        st.PauseTiming();
        Queue<T, Size> queue{queue_size};
        std::barrier barrier{2};

        std::thread producer{[&] {
            pin_current_thread(PRODUCER_CPU);
            std::array<T, BATCH_SIZE> arr{};
            for (std::size_t i = 0U; i < BATCH_SIZE; ++i) {
                arr[i] = make_value<T>(i);
            }
            auto begin = arr.begin(), end = arr.end();
            barrier.arrive_and_wait();

            for (std::size_t i = 0U; i < iterations; ++i) {
                std::size_t count{};
                while (count < BATCH_SIZE) {
                    count += queue.push_range(begin + count, end);
                }
                benchmark::DoNotOptimize(count);
            }
        }};

        st.ResumeTiming();
        barrier.arrive_and_wait();

        // Consumer
        std::size_t processed{};
        while (processed < STREAMING_SIZE) {
            std::uint64_t acc{};
            processed += queue.consume_all([&] (auto&& val) noexcept {
                acc += get_key(val);
            });
            benchmark::DoNotOptimize(acc);
        }

        producer.join();
        benchmark::ClobberMemory();
    }
    st.SetItemsProcessed(st.iterations() * STREAMING_SIZE);
    st.SetBytesProcessed(st.iterations() * STREAMING_SIZE * sizeof(T));
}

template <template <typename, std::size_t> typename Queue, typename T, std::size_t Size>
void BM_spsc_roundtrip(benchmark::State& st) {
    pin_current_thread(CONSUMER_CPU);

    for (auto _ : st) {
        st.PauseTiming();
        Queue<T, Size> out_queue{Size};
        Queue<T, Size> in_queue{Size};
        std::barrier barrier{2};

        std::thread producer{[&] {
            pin_current_thread(PRODUCER_CPU);
            barrier.arrive_and_wait();

            for (std::size_t i = 0U; i < STREAMING_SIZE; ++i) {
                auto value = make_value<T>(i);
                out_queue.emplace(std::move(value));
                in_queue.dequeue(value);
                benchmark::DoNotOptimize(value);
            }
        }};

        st.ResumeTiming();
        barrier.arrive_and_wait();

        // Consumer
        for (std::size_t i = 0U; i < STREAMING_SIZE; ++i) {
            T value;
            out_queue.dequeue(value);
            benchmark::DoNotOptimize(value);
            in_queue.emplace(std::move(value));
        }

        producer.join();
        benchmark::ClobberMemory();
    }

    st.SetItemsProcessed(st.iterations() * STREAMING_SIZE);
    st.SetBytesProcessed(st.iterations() * STREAMING_SIZE * sizeof(T));
}


// Throughput
BENCHMARK_TEMPLATE(BM_spsc_throughput, mutex_queue, value_type1, queue_size)
    ->Name("mutex_queue/throughput/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_throughput, mutex_queue, value_type2, queue_size)
    ->Name("mutex_queue/throughput/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

BENCHMARK_TEMPLATE(BM_spsc_throughput, rigtorp_spsc_queue, value_type1, queue_size)
    ->Name("rigtorp/throughput/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_throughput, rigtorp_spsc_queue, value_type2, queue_size)
    ->Name("rigtorp/throughput/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

#if URLICHT_BM_HAS_BOOST
BENCHMARK_TEMPLATE(BM_spsc_throughput, boost_spsc_queue, value_type1, queue_size)
    ->Name("boost/throughput/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_throughput, boost_spsc_queue, value_type2, queue_size)
    ->Name("boost/throughput/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
#endif

BENCHMARK_TEMPLATE(BM_spsc_throughput, urlicht_spsc_queue, value_type1, queue_size)
    ->Name("urlicht/throughput/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_throughput, urlicht_spsc_queue, value_type2, queue_size)
    ->Name("urlicht/throughput/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

// Bulk throughput
#if URLICHT_BM_HAS_BOOST
BENCHMARK_TEMPLATE(BM_spsc_bulk_throughput, boost_spsc_queue, value_type1, queue_size)
    ->Name("boost/bulk_throughput/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_bulk_throughput, boost_spsc_queue, value_type2, queue_size)
    ->Name("boost/bulk_throughput/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
#endif

BENCHMARK_TEMPLATE(BM_spsc_bulk_throughput, urlicht_spsc_queue, value_type1, queue_size)
    ->Name("urlicht/bulk_throughput/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_bulk_throughput, urlicht_spsc_queue, value_type2, queue_size)
    ->Name("urlicht/bulk_throughput/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

// Roundtrip benchmarks (excludes mutex_queue)
BENCHMARK_TEMPLATE(BM_spsc_roundtrip, rigtorp_spsc_queue, value_type1, queue_size)
    ->Name("rigtorp/roundtrip/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_roundtrip, rigtorp_spsc_queue, value_type2, queue_size)
    ->Name("rigtorp/roundtrip/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);

#if URLICHT_BM_HAS_BOOST
BENCHMARK_TEMPLATE(BM_spsc_roundtrip, boost_spsc_queue, value_type1, queue_size)
    ->Name("boost/roundtrip/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_roundtrip, boost_spsc_queue, value_type2, queue_size)
    ->Name("boost/roundtrip/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
#endif

BENCHMARK_TEMPLATE(BM_spsc_roundtrip, urlicht_spsc_queue, value_type1, queue_size)
    ->Name("urlicht/roundtrip/int")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
BENCHMARK_TEMPLATE(BM_spsc_roundtrip, urlicht_spsc_queue, value_type2, queue_size)
    ->Name("urlicht/roundtrip/info_t")
    ->UseRealTime()->Repetitions(10)->ReportAggregatesOnly(true);
