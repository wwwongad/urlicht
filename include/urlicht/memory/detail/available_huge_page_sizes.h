#ifndef URLICHT_MEMORY_DETAIL_AVAILABLE_HUGE_PAGE_SIZES_H
#define URLICHT_MEMORY_DETAIL_AVAILABLE_HUGE_PAGE_SIZES_H

#include <urlicht/internal/config.h>

#include <cstddef>
#include <vector>

#if UL_PLATFORM_LINUX
#include <algorithm>
#include <bit>
#include <charconv>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string>
#include <string_view>
#include <system_error>
#elif UL_PLATFORM_WINDOWS
#   define WIN32_LEAN_AND_MEAN
#   define NOMINMAX
#include <windows.h>
#endif

namespace urlicht::memory::detail {

    /**
     * @brief Returns huge page sizes in bytes that can currently satisfy a one-page allocation.
     *
     * Linux returns every huge page size with at least one free page, in ascending order.
     * Windows enables SeLockMemoryPrivilege for the process and returns the system's sole
     * large-page size when that succeeds. macOS returns MAP_ALIGNED_SUPER's 2 MiB alignment.
     */
    [[nodiscard]] inline std::vector<std::size_t> available_huge_page_sizes() {
#if UL_PLATFORM_LINUX
        constexpr std::string_view directory = "/sys/kernel/mm/hugepages";
        constexpr std::string_view prefix = "hugepages-";
        constexpr std::string_view suffix = "kB";

        std::vector<std::size_t> sizes;
        std::error_code ec;
        std::filesystem::directory_iterator iterator{directory, ec};
        const std::filesystem::directory_iterator end;

        while (!ec && iterator != end) {
            const std::filesystem::directory_entry& entry = *iterator;
            const std::string_view path{entry.path().native()};
            const std::size_t separator = path.rfind('/');
            const std::string_view name =
                separator == std::string_view::npos ? path : path.substr(separator + 1U);

            if (name.starts_with(prefix) && name.ends_with(suffix)) {
                unsigned long long kib = 0U;
                const char* const first = name.data() + prefix.size();
                const char* const last = name.data() + name.size() - suffix.size();

                if (const auto [parsed, parse_error] = std::from_chars(first, last, kib);
                    parse_error == std::errc{} && parsed == last && kib != 0U &&
                    kib <= std::numeric_limits<std::size_t>::max() / 1024U) {
                    if (const std::size_t size = static_cast<std::size_t>(kib) * 1024U;
                        std::has_single_bit(size)) {
                        std::ifstream free_pages{entry.path() / "free_hugepages"};
                        unsigned long long free_count = 0U;

                        if (free_pages >> free_count; free_count != 0U) {
                            sizes.push_back(size);
                        }
                    }
                }
            }
            iterator.increment(ec);
        }

        std::ranges::sort(sizes);
        sizes.erase(std::ranges::unique(sizes).begin(), sizes.end());
        return sizes;
#elif UL_PLATFORM_WINDOWS
        const SIZE_T minimum = ::GetLargePageMinimum();
        if (minimum == 0U) {
            return {};
        }

        HANDLE token = nullptr;
        if (::OpenProcessToken(::GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES | TOKEN_QUERY, &token) == 0) {
            return {};
        }

        TOKEN_PRIVILEGES privileges{};
        privileges.PrivilegeCount = 1U;
        privileges.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;
        if (::LookupPrivilegeValueW(nullptr, SE_LOCK_MEMORY_NAME, &privileges.Privileges[0].Luid) == 0) {
            ::CloseHandle(token);
            return {};
        }

        ::SetLastError(ERROR_SUCCESS);
        const BOOL adjusted_privileges =
            ::AdjustTokenPrivileges(token, FALSE, &privileges, 0, nullptr, nullptr);
        const DWORD adjust_error = ::GetLastError();
        ::CloseHandle(token);

        if (adjusted_privileges == 0 || adjust_error != ERROR_SUCCESS) {
            return {};
        }

        return {static_cast<std::size_t>(minimum)};
#else // macOS
        return {std::size_t{1U} << 21U};
#endif
    }

}

#endif // URLICHT_MEMORY_DETAIL_AVAILABLE_HUGE_PAGE_SIZES_H
