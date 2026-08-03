#include <urlicht/concurrency/mpmc_queue.h>
#include <urlicht/functional/flexible_move_only_function.h>
#include <atomic>
#include <iostream>
#include <syncstream>
#include <thread>
#include <barrier>
#include <format>

void print_task(const int task_id, const int submitter_id, const int worker_id) {
    std::osyncstream(std::cout) <<
        std::format("Completed task {} from thread {} by worker {}\n", task_id, submitter_id, worker_id);
}

using func_t = urlicht::functional::flexible_move_only_function<void(const int)>;

int main() {
    constexpr size_t num_threads_per_side = 4;
    constexpr size_t num_tasks_per_thread = 10;
    constexpr size_t num_total_tasks = num_threads_per_side * num_tasks_per_thread;

    std::barrier barrier{num_threads_per_side * 2};
    urlicht::concurrency::mpmc_queue<func_t, urlicht::concurrency::capacity<64>> task_queue;

    std::vector<std::jthread> submitters;
    for (size_t submitter = 0; submitter < num_threads_per_side; ++submitter) {
        submitters.emplace_back([&, submitter] {
            for (size_t task = 0; task < num_tasks_per_thread; ++task) {
                task_queue.emplace([submitter, task] (const int worker) {
                    const int actual_task_id = submitter * num_tasks_per_thread + task;
                    print_task(actual_task_id, submitter, worker);
                });
            }
        });
    }

    std::vector<std::jthread> workers;
    std::atomic<size_t> task_completed{};
    for (size_t i = 0; i < num_threads_per_side; ++i) {
        workers.emplace_back([&, i] {
            while (task_completed.fetch_add(1) < num_total_tasks) {
                func_t task{task_queue.dequeue()};
                task(i);
            }
        });
    }
}
