#pragma once

#include "policy.h"

#include <cmath>
#include <iostream>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

namespace iter_details {

template <class T>
constexpr bool is_const = false;
template <template <bool> class T>
constexpr bool is_const<T<true>> = true;

} // namespace iter_details

template <
        class Key,
        class T,
        class CollisionPolicy = LinearProbing,
        class Hash = std::hash<Key>,
        class Equal = std::equal_to<Key>>
class HashMap
{
public:
    // types
    using key_type = Key;
    using mapped_type = T;
    using value_type = std::pair<const Key, T>;
    using size_type = std::size_t;
    using hasher = Hash;
    using key_equal = Equal;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;

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
        friend class HashMap;
        friend class iterator;
        //        friend class const_iterator;
    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using data_t = std::conditional_t<is_const, const std::vector<std::optional<value_type>>, std::vector<std::optional<value_type>>>;
        using trash_t = std::conditional_t<is_const, const std::vector<bool>, std::vector<bool>>;
        using value_type = std::conditional_t<is_const, const value_type, value_type>;
        using pointer = value_type *;
        using reference = value_type &;

    protected:
        data_t * data = nullptr;
        trash_t * trash = nullptr;
        size_type curr_ind = 0;
        bool empty_it = true;

        base_iterator(data_t * data, trash_t * trash, size_type ind, bool empty_it = false)
            : data(data)
            , trash(trash)
            , curr_ind(ind)
            , empty_it(empty_it)
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

        reference operator*() const
        {
            return *((*data)[curr_ind]);
        }

        pointer operator->() const
        {
            return &((*data)[curr_ind].value());
        }

        base_iterator & operator++()
        {
            curr_ind++;
            while (!is_end() && ((!(*data)[curr_ind]) || (*trash)[curr_ind])) {
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

    explicit HashMap(size_type expected_max_size = 8, // ???????
                     const hasher & hash = hasher(),
                     const key_equal & equal = key_equal())
        : data(std::vector<std::optional<value_type>>(std::max(expected_max_size, size_type(8)) * 2))
        , trash(std::vector<bool>(std::max(expected_max_size, size_type(8)) * 2, false))
        , hash(hash)
        , equal(equal)
    {
    }

    template <class InputIt>
    HashMap(InputIt first, InputIt last, size_type expected_max_size = 8, const hasher & hash = hasher(), const key_equal & equal = key_equal())
        : data(std::max(expected_max_size, size_type(8)) * 2)
        , trash(std::max(expected_max_size, size_type(8)) * 2, false)
        , hash(hash)
        , equal(equal)
    {
        insert(first, last);
    }

    HashMap(const HashMap & other)
        : data(other.data)
        , trash(other.trash)
        , filled_elems(other.filled_elems)
        , deleted_elems(other.deleted_elems)
        , hash(other.hash)
        , equal(other.equal)
    {
    }

    HashMap(HashMap && other)
    {
        this->operator=(std::move(other));
    }

    HashMap(std::initializer_list<value_type> init,
            size_type expected_max_size = 8,
            const hasher & hash = hasher(),
            const key_equal & equal = key_equal())
        : data(std::max(expected_max_size, size_type(8)) * 2)
        , trash(std::max(expected_max_size, size_type(8)) * 2, false)
        , hash(hash)
        , equal(equal)
    {
        insert(init);
    }

    HashMap & operator=(const HashMap & other)
    {
        *this = HashMap(other);
        return *this;
    }

    HashMap & operator=(HashMap && other) noexcept
    {
        swap(other);
        return *this;
    }

    HashMap & operator=(std::initializer_list<value_type> init)
    {
        data.clear();
        filled_elems = 0;
        deleted_elems = 0;
        insert(init);
        return *this;
    }

    iterator begin() noexcept
    {
        if (empty()) {
            return end();
        }
        return iterator(&data, &trash, find_first_filled());
    }

    const_iterator begin() const noexcept
    {
        if (empty()) {
            return end();
        }
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

    std::pair<iterator, bool> insert(const value_type & value)
    {
        return do_emplace(value);
    }

    template <class P>
    std::pair<iterator, bool> insert(P && value)
    {
        return emplace(std::forward<P>(value));
    }

    iterator insert(const_iterator /*hint*/, const value_type & value)
    {
        return insert(value);
    }

    iterator insert(const_iterator hint, value_type && value)
    {
        return emplace_hint(hint, std::forward<value_type>(value));
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

    template <class M>
    std::pair<iterator, bool> insert_or_assign(const key_type & key, M && value)
    {
        auto tmp = emplace(key, std::forward<M>(value));
        if (!tmp.second) {
            data[tmp.first.curr_ind].value().second = std::forward<M>(value);
        }
        return tmp;
    }
    template <class M>
    std::pair<iterator, bool> insert_or_assign(key_type && key, M && value)
    {
        auto tmp = emplace(std::move(key), std::forward<M>(value));
        if (!tmp.second) {
            data[tmp.first.curr_ind].value().second = std::forward<M>(value);
        }
        return tmp;
    }

    template <class M>
    iterator insert_or_assign(const_iterator hint, const key_type & key, M && value)
    {
        if (!hint.is_end() && data[hint.curr_ind].value().first == key && data[hint.curr_ind] && !trash[hint.curr_ind]) {
            data[hint.curr_ind].value().second = mapped_type(std::forward<M>(value));
            return iterator(&data, &trash, hint.curr_ind, hint.empty_it);
        }
        return insert_or_assign(key, std::forward<M>(value)).first;
    }

    template <class M>
    iterator insert_or_assign(const_iterator hint, key_type && key, M && value)
    {
        if (!hint.is_end() && equal(data[hint.curr_ind].value().first, key) && data[hint.curr_ind] && !trash[hint.curr_ind]) {
            data[hint.curr_ind].value().second = mapped_type(std::forward<M>(value));
            return iterator(&data, &trash, hint.curr_ind, hint.empty_it);
        }
        return insert_or_assign(std::move(key), std::forward<M>(value)).first;
    }

    // construct element in-place, no copy or move operations are performed;
    // element's constructor is called with exact same arguments as `emplace` method
    // (using `std::forward<Args>(args)...`)
    template <class... Args>
    std::pair<iterator, bool> emplace(Args &&... args)
    {
        return do_emplace(std::forward<Args>(args)...);
    }

    template <class... Args>
    iterator emplace_hint(const_iterator /* hint*/, Args &&... args)
    {
        return do_emplace(std::forward<Args>(args)...).first;
    }

    template <class... Args>
    std::pair<iterator, bool> try_emplace(const key_type & key, Args &&... args)
    {
        auto tmp_it = find(key);
        if (!tmp_it.is_end()) {
            return {tmp_it, false};
        }
        collision_next.init();
        reserve(filled_elems + deleted_elems);
        auto ind = find_index(key);
        data[ind].template emplace(
                std::piecewise_construct,
                std::forward_as_tuple(key),
                std::forward_as_tuple(std::forward<Args>(args)...));
        if (trash[ind]) {
            trash[ind] = false;
            deleted_elems--;
        }
        ++filled_elems;
        return {iterator(&data, &trash, ind), true};
    }

    template <class... Args>
    std::pair<iterator, bool> try_emplace(key_type && key, Args &&... args)
    {
        auto tmp_it = find(key);
        if (!tmp_it.is_end()) {
            return {tmp_it, false};
        }
        reserve(filled_elems + deleted_elems);
        collision_next.init();
        auto ind = find_index(key);
        data[ind].template emplace(
                std::piecewise_construct,
                std::forward_as_tuple(std::move(key)),
                std::forward_as_tuple(std::forward<Args>(args)...));
        ++filled_elems;
        return {iterator(&data, &trash, ind), true};
    }

    template <class... Args>
    iterator try_emplace(const_iterator hint, const key_type & key, Args &&... args)
    {
        return emplace_hint(hint, std::piecewise_construct, std::forward_as_tuple(key), std::forward_as_tuple(std::forward<Args>(args)...));
    }

    template <class... Args>
    iterator try_emplace(const_iterator hint, key_type && key, Args &&... args)
    {
        return emplace_hint(hint, std::piecewise_construct, std::forward_as_tuple(std::move(key)), std::forward_as_tuple(std::forward<Args>(args)...));
    }

    iterator erase(const_iterator pos)
    {
        trash[pos.curr_ind] = true;
        filled_elems--;
        deleted_elems++;
        ++pos;
        return iterator(&data, &trash, pos.curr_ind, pos.empty_it);
    }

    iterator erase(const_iterator first, const_iterator last)
    {
        for (auto it = first; it != last; ++it) {
            erase(it);
        }
        ++last;
        return iterator(&data, &trash, last.curr_ind, last.empty_it);
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
    void swap(HashMap & other) noexcept
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
            if ((!trash[ind]) && equal(data[ind].value().first, key)) {
                return iterator(&data, &trash, ind);
            }
            ind = (ind + find_next.next()) % data.size();
        }
        return end();
    }

    const_iterator find(const key_type & key) const
    {
        return const_iterator(const_cast<HashMap *>(this)->find(key));
    }

    bool contains(const key_type & key) const
    {
        return find(key) != cend();
    }

    std::pair<iterator, iterator> equal_range(const key_type & key)
    {
        auto it = find(key);
        if (it.is_end()) {
            return {it, it};
        }
        return {it++, it};
    }

    std::pair<const_iterator, const_iterator> equal_range(const key_type & key) const
    {
        auto it = std::move(find(key));
        if (it.is_end()) {
            return {it, it};
        }
        return {it++, it};
    }

    mapped_type & at(const key_type & key)
    {
        auto it = find(key);
        if (it.is_end()) {
            throw std::out_of_range("not found");
        }
        return data[it.curr_ind].value().second;
    }

    const mapped_type & at(const key_type & key) const
    {
        auto it = find(key);
        if (it.is_end()) {
            throw std::out_of_range("not found");
        }
        return data[it.curr_ind].value().second;
    }

    mapped_type & operator[](const key_type & key)
    {
        return this->try_emplace(key).first->second;
    }

    mapped_type & operator[](key_type && key)
    {
        return this->try_emplace(std::move(key)).first->second;
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
            HashMap tmp = HashMap(count * 2, hash, equal);
            for (auto it = cbegin(); it != cend(); ++it) {
                if (data[it.curr_ind]) {
                    tmp.emplace(std::move(const_cast<key_type &>(data[it.curr_ind]->first)), std::move(data[it.curr_ind]->second));
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
    friend bool operator==(const HashMap & lhs, const HashMap & rhs)
    {
        if (lhs.size() != rhs.size()) {
            return false;
        }
        for (auto it = lhs.begin(); it != lhs.end(); ++it) {
            auto tmp = rhs.find(it->first);
            if (tmp.is_end() || *tmp != *it) {
                return false;
            }
        }
        for (auto it = rhs.begin(); it != rhs.end(); ++it) {
            auto tmp = lhs.find(it->first);
            if (tmp.is_end() || *tmp != *it) {
                return false;
            }
        }
        return true;
    }

    friend bool operator!=(const HashMap & lhs, const HashMap & rhs)
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
        while ((!trash[ind]) && data[ind] && !equal(key, data[ind]->first)) {
            ind = (ind + collision_next.next()) % data.size();
        }
        return ind;
    }

    template <class K, class V>
    std::pair<iterator, bool> do_emplace(K && key, V && value)
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
            data[ind].template emplace(std::forward<K>(key), std::forward<V>(value));
            filled_elems++;
            inserted = true;
        }
        return std::make_pair(iterator(&data, &trash, ind), inserted);
    }

    template <class... KArgs, class... VArgs>
    std::pair<iterator, bool> do_emplace(std::piecewise_construct_t, std::tuple<KArgs...> && to_key, std::tuple<VArgs...> && to_value)
    {
        reserve(filled_elems + deleted_elems);
        collision_next.init();
        key_type key(std::make_from_tuple<key_type>(std::forward<std::tuple<KArgs...>>(to_key)));
        if (find(key) != end()) {
            return {find(key), false};
        }
        size_type ind = find_index(key);
        bool inserted = false;
        if (!data[ind]) {
            while ((!trash[ind]) && data[ind]) {
                ind = (ind + collision_next.next()) % data.size();
            }
            if (trash[ind]) {
                trash[ind] = false;
                deleted_elems--;
            }
            data[ind].template emplace(
                    std::forward<Key>(key),
                    std::make_from_tuple<mapped_type>(std::forward<std::tuple<VArgs...>>(to_value)));
            filled_elems++;
        }
        return std::make_pair(iterator(&data, &trash, ind), inserted);
    }

    template <class P>
    std::pair<iterator, bool> do_emplace(P && value)
    {
        reserve(filled_elems + deleted_elems);
        collision_next.init();
        if (find(value.first) != end()) {
            return {find(value.first), false};
        }
        size_type ind = find_index(value.first);
        bool inserted = false;
        if (!data[ind]) {
            while ((!trash[ind]) && data[ind]) {
                ind = (ind + collision_next.next()) % data.size();
            }
            if (trash[ind]) {
                trash[ind] = false;
                deleted_elems--;
            }
            data[ind].emplace(std::forward<P>(value));
            filled_elems++;
            inserted = true;
        }
        return std::make_pair(iterator(&data, &trash, ind), inserted);
    }
};