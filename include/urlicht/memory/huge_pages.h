#ifndef URLICHT_MEMORY_HUGE_PAGES_H
#define URLICHT_MEMORY_HUGE_PAGES_H

#include <urlicht/internal/config.h>
#include <urlicht/internal/error.h>
#include <urlicht/memory/detail/arena_fwd.h> // For allocation_result
#include <cinttypes>
#include <limits>
#include <bit>
#include <format>
#include <type_traits>

#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
#include <sys/mman.h>
#elif UL_PLATFORM_WINDOWS
#include <windows.h>
#endif

#if UL_PLATFORM_MACOS
#warning "Custom huge pages are generally not supported on macOS." \
         "Allocation may silently fall back to normal 4KiB pages or fail."
#endif

namespace urlicht::memory {

    // This enum is for convenience only, not a comprehensive list of possible sizes.
    // For page sizes outside this list, use the constructor that accepts log page size.
    enum class huge_page_size : std::uint8_t {
        SIZE_64KB,
        SIZE_2MB,
        SIZE_16MB,
        SIZE_32MB,
        SIZE_256MB,
        SIZE_512MB,
        SIZE_1GB,
        SIZE_16GB
    };

    /**
      * @brief Portable memory-protection flags for huge-page allocations.
      *        Multiple flags can be combined using the OR operation.
      *        On Linux/macOS, the mask maps bit-wise onto PROT_READ/PROT_WRITE/PROT_EXEC
      *        (an empty mask maps to PROT_NONE). On Windows, the whole mask maps onto a single
      *        PAGE_* constant; write implies read and write_execute implies read, since Windows
      *        has no write-only or execute-and-write-without-read protection mode.
      */
    enum class huge_page_protection : std::uint8_t {
        none               = 0U,
        read               = 1U << 0U,
        write              = 1U << 1U,
        execute            = 1U << 2U,
        read_write         = (1U << 0U) | (1U << 1U),
        read_execute       = (1U << 0U) | (1U << 2U),
        write_execute      = (1U << 1U) | (1U << 2U),
        read_write_execute = (1U << 0U) | (1U << 1U) | (1U << 2U)
    };

    [[nodiscard]] constexpr huge_page_protection operator|(const huge_page_protection lhs,
                                                           const huge_page_protection rhs) noexcept {
        return static_cast<huge_page_protection>(static_cast<std::uint8_t>(lhs) | static_cast<std::uint8_t>(rhs));
    }

    constexpr huge_page_protection& operator|=(huge_page_protection& lhs, const huge_page_protection rhs) noexcept {
        return lhs = (lhs | rhs);
    }

    [[nodiscard]] constexpr huge_page_protection operator&(const huge_page_protection lhs,
                                                           const huge_page_protection rhs) noexcept {
        return static_cast<huge_page_protection>(static_cast<std::uint8_t>(lhs) & static_cast<std::uint8_t>(rhs));
    }

    constexpr huge_page_protection& operator&=(huge_page_protection& lhs, const huge_page_protection rhs) noexcept {
        return lhs = (lhs & rhs);
    }

    /**
      * @brief Portable allocation-option flags for huge-page allocations.
      *        Multiple flags can be combined using the OR operation. Options without a platform
      *        equivalent are silently ignored (see the per-flag notes).
      */
    enum class huge_page_allocation_options : std::uint8_t {
        none     = 0U,
        // Pre-faults all pages at allocation time (MAP_POPULATE).
        // Note: On Windows, large pages are committed and resident regardless of this option.
        // On macOS, this option has no effect.
        populate = 1U << 0U,
        // Locks the pages so they cannot be swapped out (MAP_LOCKED).
        // Note: On Windows, large pages are always locked. On macOS, this option has no effect.
        locked = 1U << 1U
    };

    [[nodiscard]] constexpr huge_page_allocation_options operator|(const huge_page_allocation_options lhs,
                                                                   const huge_page_allocation_options rhs) noexcept {
        return static_cast<huge_page_allocation_options>(
            static_cast<std::uint8_t>(lhs) | static_cast<std::uint8_t>(rhs)
        );
    }

    constexpr huge_page_allocation_options& operator|=(huge_page_allocation_options& lhs,
                                                       const huge_page_allocation_options rhs) noexcept {
        return lhs = (lhs | rhs);
    }

    [[nodiscard]] constexpr huge_page_allocation_options operator&(const huge_page_allocation_options lhs,
                                                                   const huge_page_allocation_options rhs) noexcept {
        return static_cast<huge_page_allocation_options>(
            static_cast<std::uint8_t>(lhs) & static_cast<std::uint8_t>(rhs)
        );
    }

    constexpr huge_page_allocation_options& operator&=(huge_page_allocation_options& lhs,
                                                       const huge_page_allocation_options rhs) noexcept {
        return lhs = (lhs & rhs);
    }

    /**
     * @brief A RAII wrapper for anonymous huge pages.
     *        Linux: mmap(2) with MAP_HUGETLB and an explicit MAP_HUGE_* size flag.
     *        macOS: mmap(2) with MAP_ALIGNED_SUPER (best effort; the requested page size is not guaranteed).
     *        Windows: VirtualAlloc2 with MEM_LARGE_PAGES and an explicit page-size extended parameter.
     */
    class huge_pages {
    public:
        using size_type = std::size_t;
        using log_size_type = std::uint8_t;
        using pointer = void*;
        using const_pointer = const void*;
        using allocation_result_type = detail::allocation_result_impl<pointer, size_type>;

        constexpr huge_pages() noexcept = default;

        /**
         * @brief Constructs the object with the given params.
         * @param log_page_size The log2 of the page size in bytes. For example, for 2MB pages, log_page_size = 21.
         * @param page_count The number of pages to allocate. Defaults to 1.
         * @param prot The memory protection flags as a portable protection mask.
         *        Defaults to huge_page_protection::read_write.
         * @param options Portable allocation options, OR-combinable. Defaults to huge_page_allocation_options::populate.
         *        Options without a platform equivalent are silently ignored (see huge_page_allocation_options).
         */
        huge_pages(const log_size_type log_page_size,
                   const size_type page_count = 1ULL,
                   const huge_page_protection prot = huge_page_protection::read_write,
                   const huge_page_allocation_options options = huge_page_allocation_options::populate)
        : page_size_{page_size_from_log_(log_page_size)},
          page_cnt_{page_count},
          total_size_{allocation_size_(page_size_, page_cnt_)} {
            do_allocate_(prot, options);
        }

        /**
         * @brief Constructs the object with the given params.
         * @param page_size A huge_page_size enum to indicate the size of the huge page.
         * @param page_count The number of pages to allocate. Defaults to 1.
         * @param prot The memory protection flags as a portable protection mask.
         *        Defaults to huge_page_protection::read_write.
         * @param options Portable allocation options, OR-combinable. Defaults to huge_page_allocation_options::populate.
         *        Options without a platform equivalent are silently ignored (see huge_page_allocation_options).
         */
        huge_pages(const huge_page_size page_size,
                   const size_type page_count = 1ULL,
                   const huge_page_protection prot = huge_page_protection::read_write,
                   const huge_page_allocation_options options = huge_page_allocation_options::populate)
        : huge_pages(to_log_size_(page_size), page_count, prot, options)
        {   }

        huge_pages(const huge_pages&) = delete;
        huge_pages& operator=(const huge_pages&) = delete;

        huge_pages(huge_pages&& other) noexcept
        : data_{other.data_},
          page_size_{other.page_size_},
          page_cnt_{other.page_cnt_},
          total_size_{other.total_size_} {
            other.clear_no_dealloc_();
        }

        huge_pages& operator=(huge_pages&& other) noexcept {
            if (this != &other) [[likely]] {
                clear();
                data_ = other.data_;
                page_size_ = other.page_size_;
                page_cnt_ = other.page_cnt_;
                total_size_ = other.total_size_;
                other.clear_no_dealloc_();
            }
            return *this;
        }

        ~huge_pages() noexcept {
            clear();
        }

        //************************ Modifiers ************************//

        /**
         * @brief Return a pointer to the huge pages and release the ownership.
         */
        [[nodiscard]] pointer release() noexcept {
            const pointer released = data_;
            clear_no_dealloc_();
            return released;
        }

        /**
         * @brief Release ownership by returning a std::allocation_result object or equivalent containing
         *        1. {ptr}: pointer to the huge pages
         *        2. {count}: Total allocation size in bytes
         */
        [[nodiscard]] allocation_result_type release_info() noexcept {
            const allocation_result_type result{info()};
            clear_no_dealloc_();
            return result;
        }

        /**
         * @brief Lock the huge pages allocated so that they cannot be swapped out to the disk.
         * @param ec Cleared on success; set to the OS error code on failure. No-op on Windows,
         *        on which large pages are never swapped to disk.
         * @param on_fault If true, lock only the currently resident pages; non-resident pages are locked
         *        once they are faulted in (Linux only). Defaults to false.
         */
        void lock(std::error_code& ec, [[maybe_unused]] const bool on_fault = false) const noexcept {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            if (empty()) [[unlikely]] {
                return;
            }
            const auto ret =
#if UL_PLATFORM_LINUX
                ::mlock2(data_, total_size_, on_fault ? MLOCK_ONFAULT : 0);
#elif UL_PLATFORM_MACOS
                ::mlock(data_, total_size_);
#endif
            if (ret != 0) [[unlikely]] {
                ec = urlicht::internal::capture_errno();
            }
#endif
        }

        /**
         * @brief Lock the huge pages allocated so that they cannot be swapped out to the disk.
         * @param on_fault If true, lock only the currently resident pages; non-resident pages are locked
         *        once they are faulted in (Linux only). Defaults to false.
         * @throws std::system_error if locking fails. No-op on Windows, on which large pages
         *         are never swapped to disk.
         */
        void lock([[maybe_unused]] const bool on_fault = false) const {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            std::error_code ec;
            lock(ec, on_fault);
            if (ec) [[unlikely]] {
                throw std::system_error{
                    ec,
                    std::format(
                        "Failed to lock {} huge page(s) of {} bytes "
                        "(total={} bytes, on_fault={}, native error code={})",
                        page_cnt_, page_size_, total_size_, on_fault, ec.value()
                    )
                };
            }
#endif
        }

        /**
         * @brief Allow all huge pages to be swapped out to the disk.
         * @param ec Cleared on success; set to the OS error code on failure. No-op on Windows,
         *        on which large pages are never swapped to disk.
         */
        void unlock(std::error_code& ec) const noexcept {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            if (empty()) [[unlikely]] {
                return;
            }
            if (::munlock(data_, total_size_) != 0) [[unlikely]] {
                ec = urlicht::internal::capture_errno();
            }
#endif
        }


        /**
         * @brief Allow all huge pages to be swapped out to the disk.
         * @throws std::system_error if unlocking fails. No-op on Windows, on which large pages
         *         are never swapped to disk.
         */
        void unlock() const {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            std::error_code ec;
            unlock(ec);
            if (ec) [[unlikely]] {
                throw std::system_error{
                    ec,
                    std::format(
                        "Failed to unlock {} huge page(s) of {} bytes"
                        " (total={} bytes, native error code={})",
                        page_cnt_, page_size_, total_size_, ec.value()
                    )
                };
            }
#endif
        }

        /**
         * @brief Deallocate the huge pages and allocate new ones with the specified parameters.
         * @param log_page_size The log2 of the page size in bytes. For example, for 2MB pages, log_page_size = 21.
         * @param page_count The number of pages to allocate. Defaults to 1.
         * @param prot The memory protection flags as a portable protection mask.
         *        Defaults to huge_page_protection::read_write.
         * @param options Portable allocation options, OR-combinable. Defaults to huge_page_allocation_options::populate.
         *        Options without a platform equivalent are silently ignored (see huge_page_allocation_options).
         */
        void reset(const log_size_type log_page_size,
                   const size_type page_count = 1ULL,
                   const huge_page_protection prot = huge_page_protection::read_write,
                   const huge_page_allocation_options options = huge_page_allocation_options::populate) {
            clear();
            page_size_ = page_size_from_log_(log_page_size);
            page_cnt_ = page_count;
            total_size_ = allocation_size_(page_size_, page_cnt_);
            do_allocate_(prot, options);
        }

        /**
         * @brief Deallocate the huge pages and allocate new ones with the specified parameters.
         * @param page_size A huge_page_size enum to indicate the size of the huge page.
         * @param page_count The number of pages to allocate. Defaults to 1.
         * @param prot The memory protection flags as a portable protection mask.
         *        Defaults to huge_page_protection::read_write.
         * @param options Portable allocation options, OR-combinable. Defaults to huge_page_allocation_options::populate.
         *        Options without a platform equivalent are silently ignored (see huge_page_allocation_options).
         */
        void reset(const huge_page_size page_size,
                   const size_type page_count = 1ULL,
                   const huge_page_protection prot = huge_page_protection::read_write,
                   const huge_page_allocation_options options = huge_page_allocation_options::populate) {
            reset(to_log_size_(page_size), page_count, prot, options);
        }

        void reset() noexcept {
            clear();
        }

        /**
         * @brief Deallocate the huge pages and reset the object to an empty state.
         * @note This method is noexcept and will not throw exceptions. If deallocation fails,
         *       it will assert in debug builds.
         */
        void clear() noexcept {
            if (data_ != nullptr) [[likely]] {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
                [[maybe_unused]] const int err = ::munmap(data_, total_size_);
                UL_ASSERT(err == 0, "Failed to deallocate huge pages.");
#else // Windows
                [[maybe_unused]] const BOOL res = ::VirtualFree(data_, 0, MEM_RELEASE);
                UL_ASSERT(res != 0, "Failed to deallocate huge pages.");
#endif
                clear_no_dealloc_();
            }
        }

        void swap(huge_pages& other) noexcept {
            using std::swap;
            swap(data_, other.data_);
            swap(page_size_, other.page_size_);
            swap(page_cnt_, other.page_cnt_);
            swap(total_size_, other.total_size_);
        }

        friend void swap(huge_pages& lhs, huge_pages& rhs) noexcept {
            lhs.swap(rhs);
        }

        //************************ Observers ************************//

        /**
         * @return The pointer to the huge pages.
         */
        [[nodiscard]] pointer get() const noexcept {
            return data_;
        }

        /**
         * @return The pointer to the huge pages.
         */
        [[nodiscard]] pointer data() const noexcept {
            return data_;
        }

        /**
         * @return A std::allocation_result object or equivalent containing
         *        1. {ptr}: pointer to the huge pages
         *        2. {count}: Total allocation size in bytes
         */
        [[nodiscard]] allocation_result_type info() const noexcept {
            return allocation_result_type{.ptr = data_, .count = size()};
        }

        /**
         * @return The number of pages allocated.
         */
        [[nodiscard]] size_type page_count() const noexcept {
            return page_cnt_;
        }

        /**
         * @return The size per page in bytes.
         */
        [[nodiscard]] size_type page_size() const noexcept {
            return page_size_;
        }

        /**
         * @return The total allocation size in bytes, defined by page count * page size.
         */
        [[nodiscard]] size_type size() const noexcept {
            return total_size_;
        }

        /**
         * @return Whether the object is empty (data() == nullptr).
         */
        [[nodiscard]] bool empty() const noexcept {
            return data_ == nullptr;
        }

        /**
         * @return Whether the object is not empty (data() != nullptr).
         */
        [[nodiscard]] explicit operator bool() const noexcept {
            return !empty();
        }

        // Compares the data pointers of the huge_pages instances.
        [[nodiscard]] friend bool operator==(const huge_pages& lhs, const huge_pages& rhs) noexcept {
            return lhs.data_ == rhs.data_;
        }

        // Compares the data pointers of the huge_pages instances.
        [[nodiscard]] friend auto operator<=>(const huge_pages& lhs, const huge_pages& rhs) noexcept {
            return reinterpret_cast<std::uintptr_t>(lhs.data())
                                    <=>
                   reinterpret_cast<std::uintptr_t>(rhs.data());
        }

    private:
        [[nodiscard]] static int native_protection_(const huge_page_protection prot) noexcept {
            const std::uint8_t value = static_cast<std::uint8_t>(prot);
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            int native = PROT_NONE;
            if ((value & static_cast<std::uint8_t>(huge_page_protection::read)) != 0U) {
                native |= PROT_READ;
            }
            if ((value & static_cast<std::uint8_t>(huge_page_protection::write)) != 0U) {
                native |= PROT_WRITE;
            }
            if ((value & static_cast<std::uint8_t>(huge_page_protection::execute)) != 0U) {
                native |= PROT_EXEC;
            }
            return native;
#else // Windows
            switch (prot) {
                case huge_page_protection::none:
                    return PAGE_NOACCESS;
                case huge_page_protection::read:
                    return PAGE_READONLY;
                case huge_page_protection::write:
                    [[fallthrough]]
                case huge_page_protection::read_write:
                    return PAGE_READWRITE;
                case huge_page_protection::execute:
                    return PAGE_EXECUTE;
                case huge_page_protection::read_execute:
                    return PAGE_EXECUTE_READ;
                case huge_page_protection::write_execute:
                    [[fallthrough]]
                case huge_page_protection::read_write_execute:
                    return PAGE_EXECUTE_READWRITE;
                default:
                    UL_UNREACHABLE();
            }
#endif
        }

        [[nodiscard]] static int native_allocation_options_(const huge_page_allocation_options options) noexcept {
            const std::uint8_t value = static_cast<std::uint8_t>(options);
#if UL_PLATFORM_LINUX
            int native = 0;
            if ((value & static_cast<std::uint8_t>(huge_page_allocation_options::populate)) != 0U) {
                native |= MAP_POPULATE;
            }
            if ((value & static_cast<std::uint8_t>(huge_page_allocation_options::locked)) != 0U) {
                native |= MAP_LOCKED;
            }
            return native;
#else
            // Windows large-page allocations are always reserved, and cannot be swapped out.
            // macOS has no direct equivalent of MAP_POPULATE and MAP_LOCKED.
            return 0;
#endif
        }

        static auto map_failed_() noexcept {
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            return MAP_FAILED;
#else // Windows
            return nullptr;
#endif
        }

        [[nodiscard]] static log_size_type to_log_size_(const huge_page_size size) noexcept {
            switch (size) {
                case huge_page_size::SIZE_64KB:
                    return 16U;
                case huge_page_size::SIZE_2MB:
                    return 21U;
                case huge_page_size::SIZE_16MB:
                    return 24U;
                case huge_page_size::SIZE_32MB:
                    return 25U;
                case huge_page_size::SIZE_256MB:
                    return 28U;
                case huge_page_size::SIZE_512MB:
                    return 29U;
                case huge_page_size::SIZE_1GB:
                    return 30U;
                case huge_page_size::SIZE_16GB:
                    return 34U;
                default:
                    UL_UNREACHABLE();
            }
        }

        [[nodiscard]] static size_type page_size_from_log_(const log_size_type log_size) {
            if (log_size >= std::numeric_limits<size_type>::digits) [[unlikely]] {
                throw std::invalid_argument{"Huge page log size is too large"};
            }
            if (log_size < 12U /* 4KiB */) [[unlikely]] {
                throw std::invalid_argument{"Huge page size must be >= 4KiB"};
            }
            return size_type{1U} << log_size;
        }

#if UL_PLATFORM_LINUX
        [[nodiscard]] static int map_size_flag_(const size_type page_size) noexcept {
            return (std::countr_zero(page_size) & MAP_HUGE_MASK) << MAP_HUGE_SHIFT;
        }
#endif

        [[nodiscard]] static size_type allocation_size_(const size_type page_size,
                                                        const size_type page_cnt) {
            if (page_cnt == 0U) [[unlikely]] {
                return 0U;
            }
            if (page_size > std::numeric_limits<size_type>::max() / page_cnt) [[unlikely]] {
                throw std::overflow_error{"Total allocation size overflows size_type"};
            }
            return page_size * page_cnt;
        }

        void clear_no_dealloc_() noexcept {
            data_ = nullptr;
            page_size_ = 0U;
            page_cnt_ = 0U;
            total_size_ = 0U;
        }

        // page_size_, page_cnt_, and total_size_ should be set to their correct values before calling this method
        // All states are safely cleared if an exception is thrown
        void do_allocate_(const huge_page_protection prot,
                         [[maybe_unused]] const huge_page_allocation_options options) {
            if (total_size_ == 0U) [[unlikely]] { // No-op
                return;
            }
            const auto native_prot = native_protection_(prot);
            const auto native_allocation_options = native_allocation_options_(options);
#if (UL_PLATFORM_LINUX || UL_PLATFORM_MACOS)
            const int flags = MAP_PRIVATE
                            | MAP_ANONYMOUS
#if UL_PLATFORM_LINUX
                            | MAP_HUGETLB
                            | map_size_flag_(page_size_)
#else // macOS
                            | MAP_ALIGNED_SUPER
#endif
                            | native_allocation_options;
            auto* res = ::mmap(nullptr, total_size_, native_prot, flags, -1, 0);
#else // Windows
            MEM_EXTENDED_PARAMETER param{};
            param.Type = MemExtendedParameterPageSize;
            param.ULongLong = static_cast<ULONGLONG>(page_size_);
            const int flags = MEM_LARGE_PAGES
                            | MEM_COMMIT
                            | MEM_RESERVE;
            auto* res = ::VirtualAlloc2(nullptr, nullptr, total_size_, flags, native_prot, &param, 1);
#endif
            if (res == map_failed_()) [[unlikely]] {
                const auto page_size = page_size_;
                const auto page_cnt = page_cnt_;
                const auto total_size = total_size_;
                // Clear all states for potential reuse
                clear_no_dealloc_();
                const auto ec = urlicht::internal::last_system_error();
                throw std::system_error{
                    ec,
                    std::format(
                        "Failed to allocate {} huge page(s) of {} bytes "
                        "(total={} bytes, native_prot=0x{:x}, flags=0x{:x}, native error code={})",
                        page_cnt, page_size, total_size,
                        static_cast<unsigned>(native_prot),
                        static_cast<unsigned>(flags),
                        ec.value()
                    )
                };
            }
            data_ = res;
        }

        // Data members
        void* data_{};
        size_type page_size_{0U};
        size_type page_cnt_{0U};
        size_type total_size_{0U};
    };

    namespace detail {
        template <typename>
        struct is_huge_pages : std::false_type {};
        template <>
        struct is_huge_pages<huge_pages> : std::true_type {};
    }
}

namespace urlicht {
    template <typename T>
    inline constexpr bool is_urlicht_huge_pages_v = urlicht::memory::detail::is_huge_pages<T>::value;
}

namespace std {
    template <typename T>
    struct hash;

    template <>
    struct hash<urlicht::memory::huge_pages> {
        [[nodiscard]] std::size_t operator()(const urlicht::memory::huge_pages& hp) const noexcept {
            using pointer = urlicht::memory::huge_pages::pointer;
            return hash<pointer>{}(hp.get());
        }
    };
}

#endif //URLICHT_MEMORY_HUGE_PAGES_H
