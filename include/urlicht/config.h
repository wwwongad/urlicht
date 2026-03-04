#ifndef URLICHT_CONFIG_H
#define URLICHT_CONFIG_H

#include <iostream>
#include <cstdlib>

#define URLICHT_VERSION_MAJOR 1
#define URLICHT_VERSION_MINOR 0
#define URLICHT_VERSION_PATCH 0

#define URLICHT_VERSION \
    URLICHT_VERSION_MAJOR * 10000 \
  + URLICHT_VERSION_MINOR * 100 \
  + URLICHT_VERSION_PATCH

#if __cplusplus < 202002L
#   error "C++20 or newer is required for this library."
#elif defined(_MSVC_LANG) && _MSVC_LANG < 202002L
#   error "C++20 or newer is required for this library."
#endif

#if __cplusplus >= 202600L
#   define UL_HAS_CPP26 1
#else
#   define UL_HAS_CPP26 0
#endif

#if __cplusplus >= 202302L
#   define UL_HAS_CPP23 1
#else
#   define UL_HAS_CPP23 0
#endif

#if defined(__cpp_modules)
#   define UL_HAS_MODULES 1
#else
#   define UL_HAS_MODULES 0
#endif

#ifndef UL_PLATFORM_WINDOWS
    #if defined(_WIN32) || defined(_WIN64)
    #   define UL_PLATFORM_WINDOWS 1
    #else
    #   define UL_PLATFORM_WINDOWS 0
    #endif
#endif

#ifndef UL_PLATFORM_LINUX
    #if defined(__linux__) && !defined(__ANDROID__)
    #   define UL_PLATFORM_LINUX 1
    #else
    #   define UL_PLATFORM_LINUX 0
    #endif
#endif

#ifndef UL_PLATFORM_MACOS
    #if defined(__APPLE__) && defined(__MACH__)
    #   define UL_PLATFORM_MACOS 1
    #else
    #   define UL_PLATFORM_MACOS 0
    #endif
#endif

#if UL_HAS_CPP23
#   define UL_CONSTEXPR23 constexpr
#else
#   define UL_CONSTEXPR23
#endif

#if UL_HAS_CPP26
#   define UL_CONSTEXPR26 constexpr
#else
#   define UL_CONSTEXPR26
#endif

#ifdef UL_EXCEPTIONS
    #elif defined(__GNUC__) && !defined(__EXCEPTIONS)
    #  define UL_EXCEPTIONS 0
    #elif defined(__clang__) && !defined(__cpp_exceptions)
    #  define UL_EXCEPTIONS 0
    #elif defined(_MSC_VER) && !_HAS_EXCEPTIONS
    #  define UL_EXCEPTIONS 0
    #else
    #  define UL_EXCEPTIONS 1
#endif

#if UL_EXCEPTIONS
#   define UL_TRY try
#   define UL_CATCH catch
#else
#   define UL_TRY if constexpr(true)
#   define UL_CATCH if constexpr(false)
#endif

#ifndef UL_ASSERT
    #ifndef NDEBUG
    #   define UL_ASSERT(expr, msg) \
        do { \
            if((expr) == false) { \
                std::cerr << "Assertion failed in " << __FILE__ << " at line " \
                          << __LINE__ << ": " << msg << std::endl; \
                std::abort(); \
            } \
        } while (0)
    #else
    #   define UL_ASSERT(expr, msg) void(0)
    #endif // NDEBUG
#endif // UL_ASSERT


#ifndef UL_UNREACHABLE
    #if UL_HAS_CPP23
    #   define UL_UNREACHABLE() std::unreachable()
    #elif defined(__GNUC__) || defined(__clang__)
    #   define UL_UNREACHABLE() __builtin_unreachable()
    #elif defined(_MSC_VER)
    #   define UL_UNREACHABLE() __assume(false)
    #else
    #   define UL_UNREACHABLE() std::abort()
    #endif
#endif


#ifndef UL_NO_UNIQUE_ADDRESS
    #if defined(__MSC_VER__)
    #   define UL_NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
    #else
    #   define UL_NO_UNIQUE_ADDRESS [[no_unique_address]]
    #endif
#endif


#ifndef UL_ASSUME
    #if __has_cpp_attribute(assume)
    #   define UL_ASSUME(expr) [[assume(expr)]]
    #elif defined(__clang__)
    #   define UL_ASSUME(expr) __builtin_assume(expr)
    #elif defined(__GNUC__)
    #   define UL_ASSUME(expr) ((expr) ? static_cast<void>(0) : UL_UNREACHABLE())
    #elif defined(_MSC_VER)
    #   define UL_ASSUME(expr) __assume(expr)
    #else
    #   define UL_ASSUME(expr) 0
    #endif
#endif


#ifndef UL_COLD_PATH
    #if defined(__GNUC__) || defined(__clang__)
    #   define UL_COLD_PATH __attribute__((cold))
    #else
    #   define UL_COLD_PATH
    #endif
#endif


#ifndef UL_HOT_PATH
    #if defined(__GNUC__) || defined(__clang__)
    #   define UL_HOT_PATH __attribute__((hot))
    #else
    #   define UL_HOT_PATH
    #endif
#endif

#endif // URLICHT_CONFIG_H
