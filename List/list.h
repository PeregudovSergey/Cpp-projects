#include <bits/stdc++.h>

using namespace std;

template <size_t N>
class StackStorage {
 private:
  char pool[N];
  char* last = pool;
  int idx = 0;

 public:
  StackStorage() : last(pool) {}
  char* shift(size_t n,  size_t alignment) {  
    size_t space = N; 
    void* start = reinterpret_cast<void*>(last);
    void* cur = std::align(alignment, n, start, space);    
    last = reinterpret_cast<char*>(cur) + n;  
    return reinterpret_cast<char*>(cur);
  }
};

template <typename T, size_t N>
class StackAllocator {
 private:
  StackStorage<N>* pool;

 public:
  using value_type = T;

  StackAllocator() {}
  StackAllocator(StackStorage<N>& pool) : pool(&pool) {}

  StackStorage<N>* getPool() const { return pool; }

  template <typename Type>
  struct rebind {
    typedef StackAllocator<Type, N> other;
  };

  template <typename U>
  StackAllocator(const StackAllocator<U, N>& alloc) : pool(alloc.getPool()) {}

  T* allocate(size_t n) {
    char* start = pool->shift(n * sizeof(T), alignof(T));
    return reinterpret_cast<T*>(start);
  }

  void deallocate(T*, size_t) {}

  template <typename U>
  bool operator==(const StackAllocator<U, N>& rhs) {
    return pool == rhs.pool;
  }

  template <typename U, size_t M>
  bool operator!=(const StackAllocator<U, M>& rhs) {
    return !(pool == rhs.pool);
  }

  template <typename U>
  StackAllocator& operator=(const StackAllocator<U, N>& rhs) {
    pool = rhs.getPool();
    return *this;
  }

  ~StackAllocator() {}
};

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
    Node(std::string) {}
    Node(const T& value) : value(value) {}

    Node(BaseNode* prev_, BaseNode* next_) : BaseNode(prev_, next_) {}

    Node(const std::pair<BaseNode*, BaseNode*>& baseNode)
        : BaseNode(baseNode.first, baseNode.second) {}

    Node(BaseNode* prev_, BaseNode* next_, const T& value_)
        : BaseNode(prev_, next_), value(value_) {}
  };

  Allocator allocator;
  using AllocTraits = typename std::template allocator_traits<Allocator>;
  using NodeAllocator = typename AllocTraits::template rebind_alloc<Node>;
  using NodeAllocatorTraits = typename std::allocator_traits<NodeAllocator>;
  NodeAllocator nodeAllocator;
  BaseNode value_fake; 
  BaseNode* fake;///allocate mem on stack
  size_t sz;

  void link(BaseNode* left, BaseNode* right) {
    left->next = right;
    right->prev = left;
  }

  void buildFake() {

    fake = &value_fake;
    link(fake, fake);     
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
    for (int i = 0; i < n; ++i) {
      try {
        insert(end(), value);
      } catch (...) {
        erase(begin(), end()); 
        throw; 
      }
    }    
  }

  List(const List<T, Allocator>& rhs) : allocator(), sz(0) {
    allocator =
        AllocTraits::select_on_container_copy_construction(rhs.get_allocator());
    nodeAllocator = allocator;
    buildFake();    
    
    for (auto it = rhs.begin(); it != rhs.end(); ++it) {        
      try {
        insert(end(), *it); 
      } catch (...) {
        erase(begin(), end()); 
        throw; 
      }      
    }    
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

  template <bool IsConst>
  struct CommonIterator {
    using value_type = std::conditional_t<IsConst, const T, T>;
    using pointer = std::conditional_t<IsConst, const T*, T*>;
    using reference = std::conditional_t<IsConst, const T&, T&>;
    using iterator_category = std::bidirectional_iterator_tag;
    using difference_type = std::ptrdiff_t;
    BaseNode* ptr;
    CommonIterator(BaseNode* ptr_) : ptr(ptr_) {}
    CommonIterator(const CommonIterator<false>& rhs) : ptr(rhs.ptr) {}

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

    try {
      constructNode(addNode, left, right, value);
    } catch (...) {
        NodeAllocatorTraits::deallocate(nodeAllocator, addNode, 1);
        throw; 
    }    

    left->next = addNode;
    right->prev = addNode;

    sz++;
  }

  /// const insert  
  void insert(const_iterator it, const T& value) {    
    Node* addNode = createNode();

    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr;
    
    try {
      constructNode(addNode, left, right, value);
    } catch (...) {
        NodeAllocatorTraits::deallocate(nodeAllocator, addNode, 1);
        throw; 
    }    

    left->next = addNode;
    right->prev = addNode;    

    sz++;
  }

  void erase(iterator it) {
    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr->next;

    link(left, right);     

    destroyNode(reinterpret_cast<Node*>(it.ptr));    

    --sz;
  }

  /// const erase
  void erase(const_iterator it) {
    BaseNode* left = it.ptr->prev;
    BaseNode* right = it.ptr->next;

    link(left, right);     

    destroyNode(reinterpret_cast<Node*>(it.ptr));    

    --sz;
  }

  void erase(const_iterator it, const_iterator end) {
      while (it != end) {
        erase(it++);
      }
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
