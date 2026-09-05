#ifndef URLICHT_INTERNAL_ERROR_H
#define URLICHT_INTERNAL_ERROR_H

#include <urlicht/internal/config.h>
#include <cstdint>
#include <system_error>
#include <cerrno>

#if UL_PLATFORM_WINDOWS
#   define WIN32_LEAN_AND_MEAN
#   define NOMINMAX
#include <windows.h>
#endif

namespace urlicht::internal {
    /**
     * @return The lastest errno as a std::error_code instance.
     */
    [[nodiscard]] inline std::error_code capture_errno() noexcept {
        UL_ASSERT(errno != 0, "errno is expected to be non-zero");
        return std::error_code(errno, std::generic_category());
    }

    /**
     * @return The last error code from the OS as a std::error_code instance.
     *         errno on Linux/macOS, GetLastError() on Windows.
     */
    [[nodiscard]] inline std::error_code last_system_error() noexcept {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
        return capture_errno();
#elif UL_PLATFORM_WINDOWS
        return std::error_code(::GetLastError(), std::system_category());
#endif
    }

} // namespace urlicht::internal

#endif //URLICHT_INTERNAL_ERROR_H
