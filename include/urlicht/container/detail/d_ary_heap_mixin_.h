#ifndef URLICHT_D_ARY_HEAP_MIXIN__H
#define URLICHT_D_ARY_HEAP_MIXIN__H

#include <urlicht/internal/config.h>
#include <urlicht/concepts/concepts.h>
#include <optional>

namespace urlicht::container::detail {

    template <typename Base, bool>
    class d_ary_heap_stable_mixin_ : public Base {
    public:
        using Base::Base;
        using Base::operator=;
    };

    template <typename Base>
    class d_ary_heap_stable_mixin_<Base, true> : public Base {
        using counter_type = typename Base::counter_type;
    public:
        using Base::Base;
        using Base::operator=;

        [[nodiscard]] constexpr counter_type counter() const noexcept {
            return this->counter_;
        }
    };

    // Mixin for emplace/push - although they are included in both {mutable} and {immutable} heaps,
    // return type differs. Therefore, it is the cleanest to provide them as mixin.
    template <typename, bool, bool /* Mutable */>
    class d_ary_heap_emplace_mixin_;

    template <typename Base, bool Stable>
    class d_ary_heap_emplace_mixin_<Base, Stable, true> : public d_ary_heap_stable_mixin_<Base, Stable> {
        using direct_base_ = d_ary_heap_stable_mixin_<Base, Stable>;
    protected:
        using value_type = typename Base::value_type;
        using handle_type = typename Base::handle_type;
        using const_reference = typename Base::const_reference;

    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;

        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr handle_type emplace(Args&&... args) {
            auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
            auto handle = this->make_and_append_mutable_(std::forward<Args>(args)...);
            if (this->size() > 1) [[likely]] {
                this->heapify_up_(this->size() - 1);
            }
            clear_guard.release();
            return handle;
        }

        constexpr handle_type push(const_reference value) {
            return this->emplace(value);
        }

        constexpr handle_type push(value_type&& value) {
            return this->emplace(std::move(value));
        }
    };

    template <typename Base, bool Stable>
    class d_ary_heap_emplace_mixin_<Base, Stable, false> : public d_ary_heap_stable_mixin_<Base, Stable> {
        using direct_base_ = d_ary_heap_stable_mixin_<Base, Stable>;
    protected:
        using value_type = typename Base::value_type;
        using const_reference = typename Base::const_reference;

    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;

        template <typename ...Args>
        requires std::constructible_from<value_type, Args&&...>
        constexpr void emplace(Args&&... args) {
            this->make_and_append_immutable_(std::forward<Args>(args)...);
            auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
            if (this->size() > 1) [[likely]] {
                this->heapify_up_(this->size() - 1);
            }
            clear_guard.release();
        }

        constexpr void push(const_reference value) {
            return this->emplace(value);
        }

        constexpr void push(value_type&& value) {
            return this->emplace(std::move(value));
        }
    };

    template <typename Base, bool Stable, bool /* Mutable */ >
    class d_ary_heap_mutable_mixin_
        : public d_ary_heap_emplace_mixin_<Base, Stable, false> {
        using direct_base_ = d_ary_heap_emplace_mixin_<Base, Stable, false>;
    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;
    };

    template <typename Base, bool Stable>
    class d_ary_heap_mutable_mixin_<Base, Stable, true>
        : public d_ary_heap_emplace_mixin_<Base, Stable, true> {
        using direct_base_ = d_ary_heap_emplace_mixin_<Base, Stable, true>;
    protected:
        using value_type = typename Base::value_type;
        using size_type = typename Base::size_type;
        using reference = typename Base::reference;
        using const_reference = typename Base::const_reference;
        using handle_type = typename Base::handle_type;

    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;
        // Explict inheritance
        using Base::operator[];

        template <urlicht::concepts::compatible_range<value_type> Rng,
                  std::output_iterator<handle_type> OutputIt>
        constexpr void push_range_with_handles(Rng&& rng, OutputIt o_it, bool rebuild_hint = false) {
            auto clear_guard = urlicht::internal::make_scope_guard([this] { this->clear(); });
            const auto old_size = this->size();
            const auto append_size = this->container_append_range_(std::forward<Rng>(rng), o_it);
            this->rebuild_or_heapify_up_(old_size, append_size, rebuild_hint);
            clear_guard.release();
        }

        // unchecked_/try_/[checked_]modify

        template <std::invocable<reference> F>
        constexpr void unchecked_modify(const handle_type handle, F&& fn) {
            UL_ASSERT(is_valid_handle(handle), "Invalid or expired handle.");

            const auto pos = this->index_of(handle);
            this->unchecked_modify_at(pos, std::forward<F>(fn));
        }

        template <std::invocable<reference> F>
        constexpr bool try_modify(const handle_type handle, F&& fn) {
            if (!this->is_valid_handle(handle)) [[unlikely]] {
                return false;
            }
            this->unchecked_modify(handle, std::forward<F>(fn));
            return true;
        }

        template <std::invocable<reference> F>
        constexpr void modify(const handle_type handle, F&& fn) {
            if (!this->try_modify(handle, std::forward<F>(fn))) [[unlikely]] {
                throw std::out_of_range{"urlicht::container::d_ary_heap::modify(): handle invalid or expired"};
            }
        }

        // unchecked_/try_/[checked_]promote

        // UB if the priority of the element decreased after invoking fn
        template <std::invocable<reference> F>
        constexpr void unchecked_promote(const handle_type handle, F&& fn) {
            UL_ASSERT(is_valid_handle(handle), "Invalid or expired handle.");

            const auto pos = this->index_of(handle);
            this->unchecked_promote_at(pos, std::forward<F>(fn));
        }

        template <std::invocable<reference> F>
        constexpr bool try_promote(const handle_type handle, F&& fn) {
            if (!this->is_valid_handle(handle)) [[unlikely]] {
                return false;
            }
            this->unchecked_promote(handle, std::forward<F>(fn));
            return true;
        }

        template <std::invocable<reference> F>
        constexpr void promote(const handle_type handle, F&& fn) {
            if (!try_promote(handle, std::forward<F>(fn))) [[unlikely]] {
                throw std::out_of_range{"urlicht::container::d_ary_heap::promote(): handle invalid or expired"};
            }
        }

        // unchecked_/try_/[checked_]demote

        // UB if the priority of the element increased after invoking fn
        template <std::invocable<reference> F>
        constexpr void unchecked_demote(const handle_type handle, F&& fn) {
            UL_ASSERT(is_valid_handle(handle), "Invalid or expired handle.");

            const auto pos = this->index_of(handle);
            this->unchecked_demote_at(pos, std::forward<F>(fn));
        }

        template <std::invocable<reference> F>
        constexpr bool try_demote(const handle_type handle, F&& fn) {
            if (!this->is_valid_handle(handle)) [[unlikely]] {
                return false;
            }
            this->unchecked_demote(handle, std::forward<F>(fn));
            return true;
        }

        template <std::invocable<reference> F>
        constexpr void demote(const handle_type handle, F&& fn) {
            if (!try_demote(handle, std::forward<F>(fn))) [[unlikely]] {
                throw std::out_of_range{"urlicht::container::d_ary_heap::demote(): handle invalid or expired"};
            }
        }

        // unchecked_/try_/[checked_]erase

        constexpr void unchecked_erase(const handle_type handle) {
            UL_ASSERT(this->is_valid_handle(handle), "Invalid or expired handle");
            const auto pos = this->index_of(handle);
            this->unchecked_erase_at(pos);
        }

        constexpr bool try_erase(const handle_type handle) {
            if (!this->is_valid_handle(handle)) [[unlikely]] {
                return false;
            }
            this->unchecked_erase(handle);
            return true;
        }

        constexpr void erase(const handle_type handle) {
            if (!this->try_erase(handle)) [[unlikely]] {
                throw std::out_of_range{"urlicht::container::d_ary_heap::erase(): handle invalid or expired"};
            }
        }

        // unchecked_/try_/[checked_]extract

        [[nodiscard]] constexpr value_type unchecked_extract(const handle_type handle) {
            UL_ASSERT(this->is_valid_handle(handle), "Invalid or expired handle.");
            const auto pos = this->index_of(handle);
            value_type value = std::move(this->value_of_(this->container_[pos]));
            this->unchecked_erase_at(pos);  // Erase the moved-from element
            return value;
        }

        [[nodiscard]] constexpr std::optional<value_type> try_extract(const handle_type handle) {
            if (!is_valid_handle(handle)) [[unlikely]] {
                return std::nullopt;
            }
            return unchecked_extract(handle);
        }

        [[nodiscard]] constexpr value_type extract(const handle_type handle) {
            if (!this->is_valid_handle(handle)) [[unlikely]] {
                throw std::out_of_range{"urlicht::container::d_ary_heap::extract(): handle invalid or expired"};
            }
            return this->unchecked_extract(handle);
        }

        //********************** Observers **********************//

        [[nodiscard]] constexpr bool is_valid_handle(const handle_type handle) const noexcept {
            const auto id = handle.id();
            if (id >= this->id_to_pos_map_.size()) [[unlikely]]
                return false;
            if (this->id_to_pos_map_[id] == Base::npos_)
                return false;
            if constexpr (Base::track_gen()) {
                if (handle.gen() != this->id_to_gen_map_[id])
                    return false;
            }
            return true;
        }

        [[nodiscard]] constexpr handle_type handle_at(const size_type idx) const noexcept {
            UL_ASSERT(idx < this->size(), "idx out-of-range.");
            const auto id = this->id_of_(this->container_[idx]);
            return this->make_handle_(id, this->gen_of_id_(id));
        }

        [[nodiscard]] constexpr size_type index_of(const handle_type handle) const noexcept {
            UL_ASSERT(this->is_valid_handle(handle), "Invalid or expired handle");
            return this->id_to_pos_map_[handle.id()];
        }

        // Unchecked accessor
        [[nodiscard]] constexpr const_reference operator[](const handle_type handle) const noexcept {
            UL_ASSERT(this->is_valid_handle(handle), "Invalid or expired handle.");
            return this->value_of_(this->container_[index_of(handle)]);
        }

        [[nodiscard]] constexpr const_reference at(const handle_type handle) const {
            if (!is_valid_handle(handle)) [[unlikely]] {
                throw std::out_of_range{"urlicht::container::d_ary_heap::at(): Invalid or expired handle"};
            }
            return this->value_of_(this->container_[index_of(handle)]);
        }

        // Returns the map from id to the position of the element the id represents
        [[nodiscard]] constexpr const auto& id_to_pos_map() const noexcept {
            return this->id_to_pos_map_;
        }
    };

    template <typename Base, bool Stable, bool Mutable, bool /* ReuseId */>
    class d_ary_heap_reuse_id_mixin_ : public d_ary_heap_mutable_mixin_<Base, Stable, Mutable> {
        using direct_base_ = d_ary_heap_mutable_mixin_<Base, Stable, Mutable>;
    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;
    };

    template <typename Base, bool Stable>
    // If ReuseId is true, Mutable must also be true
    class d_ary_heap_reuse_id_mixin_<Base, Stable, true, true>
        : public d_ary_heap_mutable_mixin_<Base, Stable, true> {
        using direct_base_ = d_ary_heap_mutable_mixin_<Base, Stable, true>;
    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;

        [[nodiscard]] constexpr const auto& free_id_pool() const noexcept {
            return this->free_id_pool_;
        }
    };

    template <typename Base, bool Stable, bool Mutable, bool ReuseId, bool /* TrackGen */ >
    class d_ary_heap_track_gen_mixin_
        : public d_ary_heap_reuse_id_mixin_<Base, Stable, Mutable, ReuseId> {
        using direct_base_ = d_ary_heap_reuse_id_mixin_<Base, Stable, Mutable, ReuseId>;
    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;
    };

    template <typename Base, bool Stable>
    // If TrackGen is true, Mutable and ReuseID must also be true
    class d_ary_heap_track_gen_mixin_<Base, Stable, true, true, true>
        : public d_ary_heap_reuse_id_mixin_<Base, Stable, true, true> {
        using direct_base_ = d_ary_heap_reuse_id_mixin_<Base, Stable, true, true>;
    public:
        using direct_base_::direct_base_;
        using direct_base_::operator=;

        [[nodiscard]] constexpr const auto& id_to_gen_map() const noexcept {
            return this->id_to_gen_map_;
        }
    };
}

#endif //URLICHT_D_ARY_HEAP_MIXIN__H
