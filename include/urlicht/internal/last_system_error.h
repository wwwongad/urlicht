#ifndef URLICHT_INTERNAL_LAST_SYSTEM_ERROR_H
#define URLICHT_INTERNAL_LAST_SYSTEM_ERROR_H

#include <urlicht/internal/config.h>
#include <cstdint>

#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
#include <cerrno>
#else // Windows
extern "C" __declspec(dllimport) unsigned long __stdcall GetLastError(void);
#endif

namespace urlicht::internal {
    /**
     * @return The last error code from the OS as an unsigned 32‑bit integer.
     *         errno on Linux/macOS, GetLastError() on Windows.
     */
    [[nodiscard]] inline std::uint32_t last_system_error() noexcept {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
        return static_cast<std::uint32_t>(errno);
#elif UL_PLATFORM_WINDOWS
        return static_cast<std::uint32_t>(::GetLastError());
#endif
    }

} // namespace urlicht::internal

#endif //URLICHT_INTERNAL_LAST_SYSTEM_ERROR_H
