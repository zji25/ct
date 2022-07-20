#pragma once

#include "policy.h"

#include <iostream>
#include <memory>
#include <optional>
#include <vector>

namespace iter_details {

template <class T>
constexpr bool is_const = false;
template <template <bool> class T>
constexpr bool is_const<T<true>> = true;

} // namespace iter_details

template <
        class Key,
        class CollisionPolicy = LinearProbing,
        class Hash = std::hash<Key>,
        class Equal = std::equal_to<Key>>
class HashSet
{
public:
    // types
    using key_type = Key;
    using value_type = Key;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using hasher = Hash;
    using key_equal = Equal;
    using reference = Key &;
    using const_reference = const Key &;
    using pointer = Key *;
    using const_pointer = const Key *;

private:
    std::vector<std::optional<value_type>> data;
    std::vector<bool> trash;
    CollisionPolicy collision_next;
    size_type filled_elems = 0;
    size_type deleted_elems = 0;
    hasher hash;
    key_equal equal;

    template <bool is_const>
    class base_iterator
    {
        friend class HashSet;

    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using data_t = std::conditional_t<is_const, const std::vector<std::optional<value_type>>, std::vector<std::optional<value_type>>>;
        using trash_t = std::conditional_t<is_const, const std::vector<bool>, std::vector<bool>>;
        using value_type = std::conditional_t<is_const, const value_type, value_type>;
        using pointer = value_type *;
        using reference = value_type &;

    private:
        data_t * data = nullptr;
        trash_t * trash = nullptr;
        size_type curr_ind = 0;
        bool empty_it = true;

        base_iterator(data_t * data, trash_t * trash, size_type ind)
            : data(data)
            , trash(trash)
            , curr_ind(ind)
            , empty_it(false)
        {
        }

        bool is_end() const
        {
            return empty_it || (curr_ind == data->size());
        }

    public:
        base_iterator()
        {
        }

        template <class R = base_iterator, std::enable_if_t<iter_details::is_const<R>, int> = 0>
        base_iterator(const base_iterator<false> & other)
            : data(other.data)
            , trash(other.trash)
            , curr_ind(other.curr_ind)
            , empty_it(other.empty_it)
        {
        }

        bool operator==(base_iterator const & other) const
        {
            return data == other.data && trash == other.trash && curr_ind == other.curr_ind && empty_it == other.empty_it;
        }

        bool operator!=(base_iterator const & other) const
        {
            return !(*this == other);
        }

        const_reference operator*() const
        {
            return *((*data)[curr_ind]);
        }

        const_pointer operator->() const
        {
            return &((*data)[curr_ind].value());
        }

        base_iterator & operator++()
        {
            curr_ind++;
            while (!is_end() && (!(*data)[curr_ind] || (*trash)[curr_ind])) {
                curr_ind++;
            }
            return *this;
        }

        base_iterator operator++(int)
        {
            auto tmp = *this;
            this->operator++();
            return tmp;
        }
    };

public:
    using iterator = base_iterator<false>;
    using const_iterator = base_iterator<true>;

    explicit HashSet(size_type expected_max_size = 8,
                     const hasher & hash = hasher(),
                     const key_equal & equal = key_equal())
        : data(std::max(expected_max_size, size_type(8)) * 2)
        , trash(std::max(expected_max_size, size_type(8)) * 2, false)
        , hash(hash)
        , equal(equal)
    {
    }

    template <class InputIt>
    HashSet(InputIt first, InputIt last, size_type expected_max_size = 8, const hasher & hash = hasher(), const key_equal & equal = key_equal())
        : data(std::max(expected_max_size, size_type(8)) * 2)
        , trash(std::max(expected_max_size, size_type(8)) * 2, false)
        , hash(hash)
        , equal(equal)
    {
        insert(first, last);
    }

    HashSet(const HashSet & other)
        : data(other.data)
        , trash(other.trash)
        , filled_elems(other.filled_elems)
        , deleted_elems(other.deleted_elems)
        , hash(other.hash)
        , equal(other.equal)
    {
    }

    HashSet(HashSet && other)
    {
        this->operator=(std::move(other));
    }

    HashSet(std::initializer_list<value_type> init,
            size_type expected_max_size = 10,
            const hasher & hash = hasher(),
            const key_equal & equal = key_equal())
        : data(expected_max_size * 2)
        , trash(expected_max_size * 2)
        , hash(hash)
        , equal(equal)
    {
        insert(init);
    }

    HashSet & operator=(const HashSet & other)
    {
        *this = HashSet(other);
        return *this;
    }
    HashSet & operator=(HashSet && other) noexcept
    {
        swap(other);
        return *this;
    }
    HashSet & operator=(std::initializer_list<value_type> init)
    {
        data.clear();
        filled_elems = 0;
        deleted_elems = 0;
        insert(init);
        return *this;
    }

    iterator begin() noexcept
    {
        return iterator(&data, &trash, find_first_filled());
    }

    const_iterator begin() const noexcept
    {
        return const_iterator(&data, &trash, find_first_filled());
    }

    const_iterator cbegin() const noexcept
    {
        return const_iterator(&data, &trash, find_first_filled());
    }

    iterator end() noexcept
    {
        return iterator(&data, &trash, data.size());
    }

    const_iterator end() const noexcept
    {
        return const_iterator(&data, &trash, data.size());
    }

    const_iterator cend() const noexcept
    {
        return const_iterator(&data, &trash, data.size());
    }

    bool empty() const
    {
        return filled_elems == 0;
    }

    size_type size() const
    {
        return filled_elems;
    }

    size_type max_size() const
    {
        return data.max_size();
    }

    void clear()
    {
        auto old_size = data.size();
        data.clear();
        data.resize(old_size);
        filled_elems = 0;
        deleted_elems = 0;
    }

    std::pair<iterator, bool> insert(const value_type & key)
    {
        return do_emplace(key);
    }

    std::pair<iterator, bool> insert(value_type && key)
    {
        return do_emplace(std::forward<value_type>(key));
    }

    iterator insert(const_iterator /*hint */, const value_type & key)
    {
        insert(key);
    }

    iterator insert(const_iterator hint, value_type && key)
    {
        return emplace_hint(hint, std::forward<value_type>(key));
    }

    template <class InputIt>
    void insert(InputIt first, InputIt last)
    {
        for (InputIt it = first; it != last; ++it) {
            insert(*it);
        }
    }

    void insert(std::initializer_list<value_type> init)
    {
        insert(init.begin(), init.end());
    }

    // construct element in-place, no copy or move operations are performed;
    // element's constructor is called with exact same arguments as `emplace` method
    // (using `std::forward<Args>(args)...`)
    template <class... Args>
    std::pair<iterator, bool> emplace(Args &&... args)
    {
        return do_emplace(Key(std::forward<Args>(args)...));
    }

    template <class... Args>
    iterator emplace_hint(const_iterator /*hint*/, Args &&... args)
    {
        return do_emplace(Key(std::forward<Args>(args)...)).first;
    }

    iterator erase(const_iterator pos)
    {
        trash[pos.curr_ind] = true;
        filled_elems--;
        deleted_elems++;
        ++pos;
        return iterator(&data, &trash, pos.curr_ind);
    }

    iterator erase(const_iterator first, const_iterator last)
    {
        for (auto it = first; it != last; ++it) {
            erase(it);
        }
        ++last;
        return iterator(&data, &trash, last.curr_ind);
    }

    size_type erase(const key_type & key)
    {
        auto it = find(key);
        if (it.is_end()) {
            return 0;
        }
        erase(it);
        return 1;
    }

    // exchanges the contents of the container with those of other;
    // does not invoke any move, copy, or swap operations on individual elements
    void swap(HashSet & other) noexcept
    {
        using std::swap;
        swap(collision_next, other.collision_next);
        swap(hash, other.hash);
        swap(equal, other.equal);
        swap(filled_elems, other.filled_elems);
        swap(deleted_elems, other.deleted_elems);
        swap(data, other.data);
        swap(trash, other.trash);
    }

    size_type count(const key_type & key) const
    {
        return contains(key);
    }

    iterator find(const key_type & key)
    {
        CollisionPolicy find_next;
        find_next.init();

        size_type ind = hash(key) % bucket_count();
        while (data[ind]) {
            if ((!trash[ind]) && equal(data[ind].value(), key)) {
                return iterator(&data, &trash, ind);
            }
            ind = (ind + find_next.next()) % data.size();
        }
        return end();
    }

    const_iterator find(const key_type & key) const
    {
        return const_iterator(const_cast<HashSet *>(this)->find(key));
    }

    bool contains(const key_type & key) const
    {
        return find(key) != cend();
    }

    std::pair<iterator, iterator> equal_range(const key_type & key)
    {
        auto it = std::move(find(key));
        if (it.is_end()) {
            return {it, it};
        }
        return {it++, it};
    }

    std::pair<const_iterator, const_iterator> equal_range(const key_type & key) const
    {
        auto it = find(key);
        if (it.is_end()) {
            return {it, it};
        }
        return {it++, it};
    }

    size_type bucket_count() const
    {
        return data.size();
    }

    size_type max_bucket_count() const
    {
        return data.max_size();
    }

    size_type bucket_size(const size_type) const
    {
        return 1;
    }

    size_type bucket(const key_type & key) const
    {
        return hash(key) % bucket_count();
    }

    float load_factor() const
    {
        return static_cast<float>(size()) / static_cast<float>(bucket_count());
    }

    float max_load_factor() const
    {
        return 0.5f;
    }

    void rehash(const size_type count)
    {
        if (count >= (data.size() / 2)) {
            HashSet tmp = HashSet(count * 4, hash, equal);
            for (auto it = cbegin(); it != cend(); ++it) {
                if (data[it.curr_ind]) {
                    tmp.emplace(std::move(data[it.curr_ind].value()));
                }
            }
            this->swap(tmp);
        }
    }

    void reserve(size_type count)
    {
        rehash(count);
    }

    // compare two containers contents
    friend bool operator==(const HashSet & lhs, const HashSet & rhs)
    {
        if (lhs.size() != rhs.size()) {
            return false;
        }
        for (auto it = lhs.begin(); it != lhs.end(); ++it) {
            if (!rhs.contains(*it)) {
                return false;
            }
        }
        for (auto it = rhs.begin(); it != rhs.end(); ++it) {
            if (!lhs.contains(*it)) {
                return false;
            }
        }
        return true;
    }

    friend bool operator!=(const HashSet & lhs, const HashSet & rhs)
    {
        return !(lhs == rhs);
    }

private:
    size_type find_first_filled() const
    {
        for (size_type i = 0; i < data.size(); ++i) {
            if (data[i] && !trash[i]) {
                return i;
            }
        }
        return data.size();
    }
    size_type find_index(const key_type & key)
    {
        size_type ind = bucket(key);
        while ((!trash[ind]) && data[ind] && !equal(key, data[ind].value())) {
            ind = (ind + collision_next.next()) % data.size();
        }
        return ind;
    }

    template <class K>
    std::pair<iterator, bool> do_emplace(K && key)
    {
        reserve(filled_elems + deleted_elems);
        collision_next.init();
        size_type ind = find_index(key);
        if (find(key) != end()) {
            return {find(key), false};
        }
        bool inserted = false;
        if (!data[ind]) {
            while ((!trash[ind]) && data[ind]) {
                ind = (ind + collision_next.next()) % data.size();
            }
            if (trash[ind]) {
                trash[ind] = false;
                deleted_elems--;
            }
            data[ind].template emplace(std::forward<K>(key));
            filled_elems++;
            inserted = true;
        }
        return std::make_pair(iterator(&data, &trash, ind), inserted);
    }
};
