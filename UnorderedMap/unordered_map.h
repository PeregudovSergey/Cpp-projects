#include <bits/stdc++.h>

#include <iostream>
#include <list>
#include <vector>

using namespace std;

const int maxn = 1e5; 

template <typename T, typename Allocator = std::allocator<T> >
class List {
 private:
  struct BaseNode {
    BaseNode* prev = nullptr;
    BaseNode* next = nullptr;
    BaseNode() : prev(nullptr), next(nullptr) {}
    BaseNode(BaseNode* prev_, BaseNode* next_) : prev(prev_), next(next_) {}
  };

  struct Node : BaseNode {
    T value;
    Node(const T& value) : value(value) {}

    Node(BaseNode* prev_, BaseNode* next_) : BaseNode(prev_, next_) {}

    Node(const std::pair<BaseNode*, BaseNode*>& baseNode)
        : BaseNode(baseNode.first, baseNode.second) {}

    Node(BaseNode* prev_, BaseNode* next_, const T& value_)
        : BaseNode(prev_, next_), value(value_) {}

    Node(BaseNode* prev_, BaseNode* next_, T&& value_)
        : BaseNode(prev_, next_), value(std::move(const_cast<T&>(value_))) {}
  };

  Allocator allocator;
  using AllocTraits = typename std::template allocator_traits<Allocator>;
  using NodeAllocator = typename AllocTraits::template rebind_alloc<Node>;
  using NodeAllocatorTraits = typename std::allocator_traits<NodeAllocator>;
  NodeAllocator nodeAllocator;
  BaseNode* fake = nullptr;
  size_t sz;

  void buildFake() {
    fake = reinterpret_cast<BaseNode*>(
        NodeAllocatorTraits::allocate(nodeAllocator, 1));
    fake->prev = fake;
    fake->next = fake;
  }

  BaseNode* newFake() {
    BaseNode* node = reinterpret_cast<BaseNode*>(
        NodeAllocatorTraits::allocate(nodeAllocator, 1));
    node->prev = node;
    node->next = node;
    return node;
  }

  Node* createNode() {
    Node* node = NodeAllocatorTraits::allocate(nodeAllocator, 1);
    return node;
  }

  void constructNode(Node* pt, BaseNode* prev, BaseNode* next) {
    NodeAllocatorTraits::construct(nodeAllocator, pt, prev, next);
  }

  void constructNode(Node* pt, BaseNode* prev, BaseNode* next, const T& value) {
    NodeAllocatorTraits::construct(nodeAllocator, pt, prev, next, value);
  }

  void constructNode(Node* pt, BaseNode* prev, BaseNode* next, T&& value) {
    NodeAllocatorTraits::construct(nodeAllocator, pt, prev, next,
                                   std::move(value));
  }

  void destroyNode(Node* pt) {
    NodeAllocatorTraits::destroy(nodeAllocator, pt);
    NodeAllocatorTraits::deallocate(nodeAllocator, pt, 1);
  }

 public:
  List(const Allocator& allocator_ = Allocator())
      : allocator(allocator_), nodeAllocator(allocator_), sz(0) {
    buildFake();
  }

  List(size_t n, Allocator allocator_ = Allocator())
      : allocator(allocator_), nodeAllocator(allocator_), sz(n) {
    buildFake();
    BaseNode* last = fake;
    for (size_t i = 0; i < n; ++i) {
      Node* addNode = createNode();
      try {
        constructNode(addNode, last, fake);
      } catch (...) {
        NodeAllocatorTraits::deallocate(nodeAllocator, addNode, 1);
        BaseNode* start = fake->next;
        for (size_t j = 0; j < i; ++j) {
          BaseNode* to;
          if (j != i - 1) {
            to = start->next;
          }
          destroyNode(reinterpret_cast<Node*>(start));
          start = to;
        }
        sz = 0;
        throw;
      }
      last->next = addNode;
      last = addNode;
    }
    fake->prev = last;
  }

  List(size_t n, const T& value, Allocator allocator_ = Allocator())
      : allocator(allocator_), nodeAllocator(allocator_), sz(n) {
    buildFake();
    BaseNode* last = fake;
    for (int i = 0; i < n; ++i) {
      Node* addNode = createNode();
      constructNode(addNode, last, fake, value);

      last->next = addNode;

      last = addNode;
    }
    fake->prev = last;
  }

  List(const List<T, Allocator>& rhs) : allocator(), sz(rhs.size()) {
    allocator =
        AllocTraits::select_on_container_copy_construction(rhs.get_allocator());
    nodeAllocator = allocator;
    buildFake();
    BaseNode* last = fake;
    iterator it = rhs.begin();
    for (int i = 0; i < (int)rhs.size(); ++i) {
      Node* addNode = createNode();
      try {
        constructNode(addNode, last, fake, *it);
      } catch (...) {
        NodeAllocatorTraits::deallocate(nodeAllocator, addNode, 1);
        BaseNode* start = fake->next;
        for (int j = 0; j < i; ++j) {
          BaseNode* to;
          if (j != i - 1) {
            to = start->next;
          }
          destroyNode(reinterpret_cast<Node*>(start));
          start = to;
        }
        sz = 0;
        throw;
      }

      ++it;

      last->next = addNode;

      last = addNode;
    }
    fake->prev = last;
  }

  List(List<T, Allocator>&& rhs) {
    allocator = std::move(rhs.allocator);
    nodeAllocator = std::move(rhs.nodeAllocator);
    sz = rhs.sz;
    fake = rhs.fake;
    rhs.fake = nullptr;
    rhs.sz = 0;
  }

  List& operator=(const List<T, Allocator>& rhs) {
    if (AllocTraits::propagate_on_container_copy_assignment::value) {
      allocator = rhs.get_allocator();
      nodeAllocator = allocator;
    }
    BaseNode* helpFake = newFake();

    BaseNode* last = helpFake;
    iterator it = rhs.begin();
    for (int i = 0; i < (int)rhs.size(); ++i) {
      Node* addNode = createNode();
      try {
        constructNode(addNode, last, helpFake, *it);
      } catch (...) {
        NodeAllocatorTraits::deallocate(nodeAllocator, addNode, 1);
        BaseNode* start = helpFake->next;
        for (int j = 0; j < i; ++j) {
          BaseNode* to;
          if (j != i - 1) {
            to = start->next;
          }
          destroyNode(reinterpret_cast<Node*>(start));
          start = to;
        }
        throw;
      }

      ++it;

      last->next = addNode;

      last = addNode;
    }
    helpFake->prev = last;
    while (sz) {
      pop_back();
    }
    fake = helpFake;
    sz = rhs.size();
    return *this;
  }

  List& operator=(List<T, Allocator>&& rhs) {
    allocator = std::move(rhs.allocator);
    nodeAllocator = std::move(rhs.nodeAllocator);
    sz = rhs.sz;
    fake = rhs.fake;
    rhs.fake = nullptr;
    rhs.sz = 0;
    return *this;
  }

  template <bool IsConst>
  struct CommonIterator {
    using value_type = std::conditional_t<IsConst, const T, T>;
    using pointer = std::conditional_t<IsConst, const T*, T*>;
    using reference = std::conditional_t<IsConst, const T&, T&>;
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = std::ptrdiff_t;
    BaseNode* ptr;
    CommonIterator() : ptr(nullptr) {}
    CommonIterator(BaseNode* ptr_) : ptr(ptr_) {}
    CommonIterator(const CommonIterator<false>& rhs) : ptr(rhs.ptr) {}
    CommonIterator(const CommonIterator<true>& rhs) : ptr(rhs.ptr) {}

    CommonIterator<IsConst>& operator--() {
      ptr = ptr->prev;
      return *this;
    }

    CommonIterator<IsConst>& operator++() {
      ptr = ptr->next;
      return *this;
    }

    CommonIterator<IsConst> operator++(int) {
      CommonIterator it = *this;
      ptr = ptr->next;
      return it;
    }

    reference operator*() { return (reinterpret_cast<Node*>(ptr))->value; }

    template <bool IsRhsConst>
    bool operator==(const CommonIterator<IsRhsConst>& rhs) const {
      return ptr == rhs.ptr;
    }

    template <bool IsRhsConst>
    bool operator!=(const CommonIterator<IsRhsConst>& rhs) const {
      return ptr != rhs.ptr;
    }
  };

  using iterator = CommonIterator<false>;
  using const_iterator = CommonIterator<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  iterator begin() const { return iterator(fake->next); }

  const_iterator cbegin() const { return const_iterator(fake->next); }

  reverse_iterator rbegin() const { return reverse_iterator(fake); }

  const_reverse_iterator crbegin() const {
    return const_reverse_iterator(fake);
  }

  iterator end() const { return iterator(fake); }

  const_iterator cend() const { return const_iterator(fake); }

  reverse_iterator rend() const { return reverse_iterator(fake->next); }

  const_reverse_iterator crend() const {
    return const_reverse_iterator(fake->next);
  }

  Allocator get_allocator() const { return allocator; }

  size_t size() const { return sz; }

  void insert(iterator it, const T& value) {
    Node* addNode = createNode();

    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr;

    constructNode(addNode, left, right, value);

    left->next = addNode;
    right->prev = addNode;

    sz++;
  }

  void insert(iterator it, T&& value) {
    Node* addNode = createNode();

    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr;

    constructNode(addNode, left, right, std::move(value));

    left->next = addNode;
    right->prev = addNode;

    sz++;
  }

  /// const insert
  void insert(const_iterator it, const T& value) {
    Node* addNode = createNode();

    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr;

    constructNode(addNode, left, right, value);

    left->next = addNode;
    right->prev = addNode;

    sz++;
  }

  void erase(iterator it) {
    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr->next;

    left->next = right;
    right->prev = left;

    destroyNode(reinterpret_cast<Node*>(it.ptr));

    --sz;
  }

  /// const erase
  void erase(const_iterator it) {
    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr->next;

    left->next = right;
    right->prev = left;

    destroyNode(reinterpret_cast<Node*>(it.ptr));

    --sz;
  }

  void push_back(const T& value) { insert(end(), value); }

  void push_front(const T& value) { insert(begin(), value); }

  void pop_back() { erase(iterator(fake->prev)); }

  void pop_front() { erase(begin()); }

  ~List() {
    while (sz) {
      pop_back();
    }
  }
};

template <typename Key, typename Value, typename Hash = std::hash<Key>,
          typename Equal = std::equal_to<Key>,
          typename Alloc = std::allocator<std::pair<const Key, Value> > >
class UnorderedMap {
 private:
  using NodeType = std::pair<const Key, Value>;
  struct Node {
    NodeType kv;
    size_t cached_hash;
    Node(const NodeType& kv, size_t cached_hash)
        : kv(kv), cached_hash(cached_hash) {}
    Node(const NodeType& kv) : kv(kv) { cached_hash = Hash{}(kv.first); }
    Node(const Key& key, const Value& value)
        : kv(key, value), cached_hash(Hash{}(key)) {}
    Node(Key&& key, Value&& value)
        : kv(std::move(key), std::move(value)), cached_hash(Hash{}(key)) {}
    Node(NodeType&& kv)
        : kv(std::move(const_cast<Key&>(kv.first)),
             std::move(const_cast<Value&>(kv.second))) {
      cached_hash = Hash{}(kv.first);
    }

    Node(const Node& ms) : kv(ms.kv), cached_hash(ms.cached_hash) {}
    Node(Node&& ms)
        : kv(std::move(const_cast<Key&>(ms.kv.first)), std::move(ms.kv.second)),
          cached_hash(ms.cached_hash) {}
  };

  using AllocTraits = typename std::template allocator_traits<Alloc>;
  using NodeAllocator = typename AllocTraits::template rebind_alloc<Node>;
  using IteratorList = typename List<Node, NodeAllocator>::iterator;
  List<Node, NodeAllocator> elements;
  std::vector<IteratorList> link;

  auto find_first(size_t pos, const Key& key) {
    for (auto it = link[pos]; it != elements.end(); ++it) {
      if ((*it).cached_hash % link.size() != pos) {
        return make_pair(it, 0);        
      }
      if (Equal{}((*it).kv.first, key)) {
        return make_pair(it, 1); 
        //return (*it).kv.second;
      }
    }    
    return make_pair(elements.end(), 2); 
  }
 public:
  UnorderedMap(const Alloc& alloc = Alloc())
      : elements(NodeAllocator(alloc)), link(maxn, elements.end()) {}
  UnorderedMap(const UnorderedMap<Key, Value, Hash, Equal, Alloc>& map)
      : elements(), link(maxn, elements.end()) {
    for (const auto& x : map.elements) {
      insert(x.kv);
    }
  }

  UnorderedMap(UnorderedMap<Key, Value, Hash, Equal, Alloc>&& map)
      : elements(std::move(map.elements)), link(std::move(map.link)) {}

  UnorderedMap& operator=(UnorderedMap&& map) {
    clear();
    elements = std::move(map.elements);
    link = std::move(map.link);
    return *this;
  }

  void clear() {
    while (size()) {
      erase(begin());
    }
    link.clear();
  }

  template <bool IsConst>
  struct CommonIterator {
    using T =
        std::conditional_t<IsConst,
                           typename List<Node, NodeAllocator>::const_iterator,
                           typename List<Node, NodeAllocator>::iterator>;

    using value_type = std::conditional_t<IsConst, const NodeType, NodeType>;
    using reference = std::conditional_t<IsConst, const NodeType&, NodeType&>;
    using pointer = std::conditional_t<IsConst, const NodeType*, NodeType*>;
    using iterator_category = std::forward_iterator_tag;
    using difference_type = std::ptrdiff_t;

    T it;

    CommonIterator(const T& it) : it(it) {}
    CommonIterator(const CommonIterator<false>& rhs) : it(rhs.it) {}

    CommonIterator& operator++() {
      ++it;
      return *this;
    }

    CommonIterator operator++(int) {
      T it1 = it;
      ++it;
      return it1;
    }

    reference operator*() { return (*it).kv; }

    pointer operator->() { return &((*it).kv); }

    template <bool IsRhsConst>
    bool operator==(const CommonIterator<IsRhsConst>& rhs) const {
      return it == rhs.it;
    }

    template <bool IsRhsConst>
    bool operator!=(const CommonIterator<IsRhsConst>& rhs) const {
      return it != rhs.it;
    }
  };

  using Iterator = CommonIterator<false>;
  using ConstIterator = CommonIterator<true>;

  Iterator begin() const { return Iterator(elements.begin()); }

  ConstIterator cbegin() const { return ConstIterator(elements.cbegin()); }

  Iterator end() const { return Iterator(elements.end()); }

  ConstIterator cend() const { return ConstIterator(elements.cend()); }

  size_t size() { return elements.size(); }

  Value& operator[](const Key& key) {    
    size_t cached_hash = Hash{}(key);
    size_t pos = cached_hash % link.size();
    auto state = find_first(pos, key); 
    bool ok = (link[pos] == elements.end());     
    auto it = state.first; 

    if (state.second == 0) {
      elements.insert(it, Node(std::make_pair(key, Value()), cached_hash));
        auto it1 = it;
        --it1;
        return (*it).kv.second;
    }
    if (state.second == 1) {      
      return (*it).kv.second;      
    } 

    elements.insert(elements.end(),
                    Node(std::make_pair(key, Value()), cached_hash));
    if (ok) {      
      link[pos] = --elements.end();
    }
    return (*(--elements.end())).kv.second;

  }

  Value& at(const Key& key) {
    size_t cached_hash = Hash{}(key);
    size_t pos = cached_hash % link.size();    
    for (auto it = link[pos]; it != elements.end(); ++it) {
      if ((*it).cached_hash % link.size() != pos) {
        throw std::out_of_range("blah");
      }
      if (Equal{}((*it).kv.first, key)) {
        return (*it).kv.second;
      }
    }
    throw std::out_of_range("blah");
  }

  const Value& at(const Key& key) const {    
    size_t cached_hash = Hash{}(key);
    size_t pos = cached_hash % link.size();
    for (auto it = link[pos]; it != elements.end(); ++it) {
      if ((*it).cached_hash % link.size() != pos) {
        throw std::out_of_range("blah");
      }
      if (Equal{}((*it).kv.first, key)) {
        return (*it).kv.second;
      }
    }
    throw std::out_of_range();
  }

  Iterator find(const Key& key) {
    size_t cached_hash = Hash{}(key);
    size_t pos = cached_hash % link.size();
    auto state = find_first(pos, key); 
    auto it = state.first; 
    if (state.second == 0 || state.second == 2) {
      return Iterator(elements.end());
    }

    return Iterator(it);    
  }

  template <typename... Args>
  std::pair<Iterator, bool> emplace(Args&&... args) {
    Alloc alloc = Alloc();
    NodeType* pt = AllocTraits::allocate(alloc, 1);
    AllocTraits::construct(alloc, pt, std::forward<Args>(args)...);
    Node add(std::move(*pt));
    size_t pos = add.cached_hash % link.size();  
    bool ok = (link[pos] == elements.end());
    auto state = find_first(pos, add.kv.first); 
    auto it = state.first; 

    if (state.second == 0) {
      elements.insert(it, std::move(Node(std::move(std::make_pair(
                                std::move(const_cast<Key&>(add.kv.first)),
                                std::move(add.kv.second))))));
        auto it1 = it;
        --it1;
        return std::make_pair(Iterator(it1), true);
    }

    if (state.second == 1) {
      return std::make_pair(Iterator(it), false);
    }

    elements.insert(elements.end(), std::move(add));
    if (ok) {
      link[pos] = (--elements.end());
    }

    return std::make_pair(Iterator(--elements.end()), true);
  }

  std::pair<Iterator, bool> insert(const NodeType& kv) { return emplace(kv); }

  std::pair<Iterator, bool> insert(NodeType&& kv) {
    return emplace(std::move(const_cast<Key&>(kv.first)), std::move(kv.second));
  }

  template <typename Iterator>
  void insert(const Iterator& itL, const Iterator& itR) {
    Iterator it = itL;
    while (it != itR) {
      insert(*it);
      ++it;
    }
  }

  template <bool isConst>
  void erase(CommonIterator<isConst> pos) {
    auto it = pos.it;
    size_t cached_hash = (*it).cached_hash;
    int idx = cached_hash % link.size();
    if (link[idx] == it) {
      auto it1 = it;
      ++it1;
      if (it1 != elements.end() || (int)((*it1).cached_hash % link.size()) == idx) {
        link[idx] = it1;
      } else {
        link[idx] = elements.end();
      }
    }
    elements.erase(it);
  }

  void erase(ConstIterator first, ConstIterator second) {
    while (first != second) {
      ConstIterator to = first;
      ++to;
      erase(first);
      first = to;
    }
  }

  void reserve(int) {}
  ~UnorderedMap() { clear(); }
};
