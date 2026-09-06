#include <urlicht/memory/huge_pages.h>

#include <gtest/gtest.h>

#include <bit>
#include <charconv>
#include <compare>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <functional>
#include <limits>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>

namespace um = urlicht::memory;

#if UL_PLATFORM_LINUX
// Scans /sys/kernel/mm/hugepages/ for a page size that has at least one reserved huge page.
std::optional<std::size_t> reserved_huge_page_size() {
    namespace fs = std::filesystem;
    constexpr std::string_view prefix = "hugepages-";
    constexpr std::string_view suffix = "kB";

    for (std::error_code ec; const auto& entry : fs::directory_iterator("/sys/kernel/mm/hugepages", ec)) {
        const std::string name = entry.path().filename().string(); // e.g. "hugepages-2048kB"
        if (!name.starts_with(prefix) || !name.ends_with(suffix)) {
            continue;
        }
        unsigned long long kib = 0;
        const char* const first = name.data() + prefix.size();
        if (const char* const last = name.data() + name.size() - suffix.size();
            std::from_chars(first, last, kib).ec != std::errc{} || kib == 0U) {
            continue;
        }
        std::ifstream nr_file(entry.path() / "nr_hugepages");
        if (unsigned long long reserved = 0; !(nr_file >> reserved) || reserved == 0U) {
            continue;
        }
        return static_cast<std::size_t>(kib) * 1024ULL;
    }
    return std::nullopt;
}

#elif UL_PLATFORM_WINDOWS

// Large-page allocation requires SeLockMemoryPrivilege; attempt to enable it for this process.
std::optional<std::size_t> huge_page_size_on_host() {
    const SIZE_T minimum = ::GetLargePageMinimum();
    if (minimum == 0) {
        return std::nullopt;
    }
    HANDLE token = nullptr;
    if (::OpenProcessToken(::GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &token) == 0) {
        return std::nullopt;
    }
    TOKEN_PRIVILEGES privileges{};
    privileges.PrivilegeCount = 1;
    privileges.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
    if (::LookupPrivilegeValueW(nullptr, SE_LOCK_MEMORY_NAME, &privileges.Privileges[0].Luid) == 0 ||
        ::AdjustTokenPrivileges(token, FALSE, &privileges, 0, nullptr, nullptr) == 0 ||
        ::GetLastError() != ERROR_SUCCESS) {
        ::CloseHandle(token);
        return std::nullopt;
    }
    ::CloseHandle(token);
    return static_cast<std::size_t>(minimum);
}

#else // macOS

std::optional<std::size_t> huge_page_size_on_host() {
    // MAP_ALIGNED_SUPER is best effort; allocation may still fall back to 4KiB pages.
    return std::size_t{1U} << 21U;
}

#endif

// Returns allocated huge pages, or nullopt when huge pages are unavailable on this host.
std::optional<um::huge_pages> try_make_huge_pages() {
    const auto page_size = reserved_huge_page_size();
    if (!page_size) {
        return std::nullopt;
    }
    try {
        const auto log_size = static_cast<um::huge_pages::log_size_type>(std::countr_zero(*page_size));
        return um::huge_pages{log_size, 1U, um::protection::read_write, um::allocation_options::none};
    } catch (...) {
        return std::nullopt;
    }
}


#define SKIP_IF_NO_HUGE_PAGES(maybe) \
    if (!maybe) GTEST_SKIP() << "No huge pages reserved on this host " \
                             << "(Linux: echo N > /proc/sys/vm/nr_hugepages; " \
                             << "Windows: run elevated so SeLockMemoryPrivilege is available)"


void free_released(um::huge_pages::pointer ptr, const um::huge_pages::size_type size) noexcept {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
    ::munmap(ptr, size);
#else // Windows
    ::VirtualFree(ptr, 0, MEM_RELEASE);
#endif
}

TEST(HugePages, FlagEnumsOrCombinable) {
    using um::allocation_options;
    using um::protection;

    static_assert((protection::read | protection::write) == protection::read_write);
    static_assert((protection::read | protection::execute) == protection::read_execute);
    static_assert((protection::write | protection::execute) == protection::write_execute);
    static_assert((protection::read | protection::write | protection::execute)
                  == protection::read_write_execute);
    static_assert((protection::read_write & protection::read) == protection::read);
    static_assert((protection::read & protection::write) == protection::none);

    auto prot = protection::none;
    prot |= protection::read;
    prot |= protection::execute;
    EXPECT_EQ(prot, protection::read_execute);
    prot &= protection::read;
    EXPECT_EQ(prot, protection::read);

    static_assert((allocation_options::populate | allocation_options::locked) != allocation_options::none);
    static_assert((allocation_options::populate & allocation_options::none) == allocation_options::none);

    auto options = allocation_options::none;
    options |= allocation_options::locked;
    EXPECT_EQ(options, allocation_options::locked);
}

void check_empty(const um::huge_pages& hp) noexcept {
    EXPECT_TRUE(hp.empty());
    EXPECT_FALSE(hp);
    EXPECT_EQ(hp.get(), nullptr);
    EXPECT_EQ(hp.data(), nullptr);
    EXPECT_EQ(hp.size(), 0U);
}

TEST(HugePages, DefaultConstruction) {
    const um::huge_pages hp;
    check_empty(hp);
    EXPECT_EQ(hp.page_size(), 0U);
    EXPECT_EQ(hp.page_count(), 0U);
}

TEST(HugePages, ZeroPageCount) {
    um::huge_pages hp;
    auto get_zero_page = [] {
        return um::huge_pages{21U, 0U};
    };
    EXPECT_NO_THROW(hp = get_zero_page());
    check_empty(hp);
}

TEST(HugePages, InvalidLogSize) {
    EXPECT_THROW(um::huge_pages(11U), std::invalid_argument); // Below 4KiB
    EXPECT_THROW(um::huge_pages(64U), std::invalid_argument); // Exceeds the bit width of size_type
}

TEST(HugePages, OverflowingPageCount) {
    constexpr auto max_count = std::numeric_limits<std::size_t>::max();
    EXPECT_THROW(um::huge_pages(21U, max_count), std::overflow_error);
}

TEST(HugePages, HugePagesInfo) {
    const auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    const auto& hp = *maybe;

    EXPECT_TRUE(hp);
    EXPECT_FALSE(hp.empty());
    EXPECT_NE(hp.get(), nullptr);
    EXPECT_EQ(hp.data(), hp.get());
    EXPECT_EQ(hp.page_count(), 1U);
    EXPECT_GT(hp.page_size(), 0U);
    EXPECT_EQ(hp.size(), hp.page_size() * hp.page_count());

    const auto [ptr, count] = hp.info();
    EXPECT_EQ(ptr, hp.get());
    EXPECT_EQ(count, hp.size());

    EXPECT_EQ(std::hash<um::huge_pages>{}(hp), std::hash<void*>{}(hp.get()));
}

TEST(HugePages, MoveSemantics) {
    auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    um::huge_pages source = std::move(*maybe);
    const auto ptr = source.get();
    const auto total = source.size();

    um::huge_pages moved{std::move(source)};
    EXPECT_TRUE(source.empty());
    EXPECT_EQ(source.get(), nullptr);
    EXPECT_EQ(source.size(), 0U);
    EXPECT_EQ(moved.get(), ptr);
    EXPECT_EQ(moved.size(), total);

    um::huge_pages assigned;
    assigned = std::move(moved);
    EXPECT_TRUE(moved.empty());
    EXPECT_EQ(assigned.get(), ptr);
    EXPECT_EQ(assigned.size(), total);

    // Self move-assignment must be a no-op.
    um::huge_pages& self = assigned;
    assigned = std::move(self);
    EXPECT_EQ(assigned.get(), ptr);
    EXPECT_EQ(assigned.size(), total);
}

TEST(HugePages, Release) {
    auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    um::huge_pages hp = std::move(*maybe);
    const auto ptr = hp.get();
    const auto total = hp.size();

    const auto released = hp.release();
    EXPECT_EQ(released, ptr);
    EXPECT_TRUE(hp.empty());
    free_released(released, total);

    auto maybe2 = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe2);
    um::huge_pages hp2 = std::move(*maybe2);
    const auto total2 = hp2.size();
    const auto info = hp2.release_info();
    EXPECT_NE(info.ptr, nullptr);
    EXPECT_EQ(info.count, total2);
    EXPECT_TRUE(hp2.empty());
    free_released(info.ptr, info.count);
}

TEST(HugePages, Swap) {
    auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    um::huge_pages hp = std::move(*maybe);
    const auto ptr = hp.get();
    const auto total = hp.size();

    um::huge_pages other;
    hp.swap(other);
    EXPECT_TRUE(hp.empty());
    EXPECT_EQ(other.get(), ptr);
    EXPECT_EQ(other.size(), total);

    swap(other, hp);
    EXPECT_EQ(hp.get(), ptr);
    EXPECT_EQ(hp.size(), total);
    EXPECT_TRUE(other.empty());
}

TEST(HugePages, Reset) {
    auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    um::huge_pages hp = std::move(*maybe);
    const auto log_size =
        static_cast<um::huge_pages::log_size_type>(std::countr_zero(hp.page_size()));

    hp.reset();
    EXPECT_TRUE(hp.empty());

    EXPECT_NO_THROW(hp.reset(log_size, 1U));
    EXPECT_TRUE(hp);
    EXPECT_EQ(hp.page_count(), 1U);

    EXPECT_NO_THROW(hp.reset(log_size, 0U)); // Zero page count leaves the object empty
    EXPECT_TRUE(hp.empty());
}

TEST(HugePages, LockUnlockRoundTrip) {
    std::error_code ec;

    const um::huge_pages empty;
    empty.lock(ec); // No-op on empty objects (and on Windows)
    EXPECT_FALSE(ec);

    const auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    const auto& hp = *maybe;
    hp.lock(ec);
    if (!ec) {
        hp.unlock(ec);
        EXPECT_FALSE(ec);
    } else {
        // E.g. RLIMIT_MEMLOCK exhausted; the locking path itself is still exercised.
        GTEST_SKIP() << "mlock failed: " << ec.message();
    }
}

TEST(HugePages, Comparison) {
    const um::huge_pages e1;
    const um::huge_pages e2;
    EXPECT_TRUE(e1 == e2);
    EXPECT_TRUE((e1 <=> e2) == std::strong_ordering::equal);

    const auto maybe = try_make_huge_pages();
    SKIP_IF_NO_HUGE_PAGES(maybe);
    const auto& hp = *maybe;
    EXPECT_FALSE(hp == e1);
    EXPECT_TRUE((hp <=> e1) == std::strong_ordering::greater);
    EXPECT_TRUE((e1 <=> hp) == std::strong_ordering::less);
}

