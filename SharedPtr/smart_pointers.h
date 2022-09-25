#include <bits/stdc++.h>

using namespace std;

struct BaseControlBlock {
  size_t shared_count = 0;
  size_t weak_count = 0;  
  virtual void destroy() {}
  virtual void free_memory() {}
  virtual void* get_ptr() {
    return nullptr;
  }
};

template <typename T, typename Allocator = std::allocator<T>>
struct SharedControlBlock : BaseControlBlock {
  T value;
  Allocator alloc;

  template <typename... Args>
  SharedControlBlock(const Allocator& alloc,
                     Args&&... args)
      : value(std::forward<Args>(args)...), alloc(alloc) {}

  void destroy() override {
    using AllocTraits = typename std::template allocator_traits<Allocator>;
    using SharedAllocator = typename AllocTraits::template rebind_alloc<T>;
    using SharedAllocatorTraits =
        typename std::template allocator_traits<SharedAllocator>;

    SharedAllocator sharedAlloc = alloc;
    SharedAllocatorTraits::destroy(sharedAlloc, &value);
  }

  void free_memory() override {
    using AllocTraits = typename std::template allocator_traits<Allocator>;
    using SharedAllocator = typename AllocTraits::template rebind_alloc<
        SharedControlBlock<T, Allocator>>;
    using SharedAllocatorTraits =
        typename std::template allocator_traits<SharedAllocator>;

    SharedAllocator sharedAlloc = alloc;
    SharedAllocatorTraits::deallocate(sharedAlloc, this, 1);
    alloc.~Allocator(); 
  }

  void* get_ptr() override {
    return &value; 
  }
};

template <typename T, typename Allocator = std::allocator<T>,
          typename Deleter = std::default_delete<T>>
struct RegularControlBlock : BaseControlBlock {
  T* ptr;
  Allocator alloc;
  Deleter del;

  RegularControlBlock(T* ptr,
                      const Allocator& alloc, const Deleter& del)
      : ptr(ptr), alloc(alloc),
        del(del) {}

  void destroy() override { 
    del(ptr); 
    del.~Deleter(); 
  }

  void free_memory() override {
    using AllocTraits = typename std::template allocator_traits<Allocator>;
    using RegularAllocator = typename AllocTraits::template rebind_alloc<
        RegularControlBlock<T, Allocator, Deleter>>;
    using RegularAllocatorTraits =
        typename std::template allocator_traits<RegularAllocator>;
    RegularAllocator regularAlloc = alloc;
    RegularAllocatorTraits::deallocate(regularAlloc, this, 1);
    alloc.~Allocator(); 
  }
  
  void* get_ptr() override {
    return ptr; 
  }
};

template <typename T> class WeakPtr;

template <typename T> class SharedPtr;

template <typename T> class SharedPtr {
private:
  BaseControlBlock* block;
  T* ptr;

  template <typename U> friend class WeakPtr;

  template <typename U> friend class SharedPtr;

  template <typename U, typename Allocator, typename... Args>
  friend SharedPtr<U> allocateShared(Allocator alloc, Args&&... args);

  template <typename U, typename... Args>
  friend SharedPtr<U> makeShared(Args&&... args);

  template <typename Allocator>
  SharedPtr(SharedControlBlock<T, Allocator>* block)
      : block(block), ptr(&(block->value)) {}

  /// uses for lock in weak_ptr
  SharedPtr(BaseControlBlock* block, T* ptr) : block(block), ptr(ptr) {
    if (block)
      ++(block->shared_count);
  }

public:
  SharedPtr() : block(nullptr), ptr(nullptr) {}

  void reset() {
    SharedPtr<T> copy;
    std::swap(block, copy.block);
    std::swap(ptr, copy.ptr);
  }

  template <typename U> void reset(U* ptr) {
    SharedPtr<T> copy = ptr;
    std::swap(block, copy.block);
    std::swap(ptr, copy.ptr);
  }

  T* get() const { return ptr; }

  void swap(SharedPtr<T>& other) {
    std::swap(block, other.block);
    std::swap(ptr, other.ptr);
  }

  template <typename U, typename Deleter = std::default_delete<T>,
            typename Allocator = std::allocator<T>>
  SharedPtr(U* ptrU, const Deleter& del = Deleter(),
            Allocator alloc = Allocator())
      : ptr(ptrU) {
    using AllocTraits = typename std::template allocator_traits<Allocator>;
    using RegularAllocator = typename AllocTraits::template rebind_alloc<
        RegularControlBlock<T, Allocator, Deleter>>;
    using RegularAllocatorTraits =
        typename std::template allocator_traits<RegularAllocator>;

    RegularAllocator regularAlloc = alloc;
    RegularControlBlock<T, Allocator, Deleter>* pt =
        RegularAllocatorTraits::allocate(regularAlloc, 1);

    block = new (pt)
        RegularControlBlock<T, Allocator, Deleter>(ptr, alloc, del);
    (block->shared_count)++;
    /// RegularAllocatorTraits::construct(regularAlloc, pt, 1, 0, ptr,
    /// std::move(alloc), del); зачем нам тогда аллокатор, если так нельзя?????
  }

  SharedPtr(const SharedPtr<T>& other) : block(other.block), ptr(other.ptr) {
    if (block) {
      ++(block->shared_count);
    }
  }

  template <typename U>
  SharedPtr(const SharedPtr<U>& other) : block(other.block), ptr(other.ptr) {
    if (block) {
      ++(block->shared_count);
    }
  }

  template <typename U>
  SharedPtr(SharedPtr<U>&& other) : block(other.block), ptr(other.ptr) {
    other.block = nullptr;
    other.ptr = nullptr;
  }

  SharedPtr<T>& operator=(const SharedPtr<T>& other) {
    SharedPtr<T> copy = other;
    std::swap(block, copy.block);
    std::swap(ptr, copy.ptr);

    return *this;
  }

  template <typename U> SharedPtr<T>& operator=(const SharedPtr<U>& other) {
    SharedPtr<T> copy = other;
    std::swap(block, copy.block);
    std::swap(ptr, copy.ptr);

    return *this;
  }

  template <typename U> SharedPtr<T>& operator=(SharedPtr<U>&& other) {
    SharedPtr<T> copy = std::move(other);
    std::swap(block, copy.block);
    std::swap(ptr, copy.ptr);

    return *this;
  }

  T& operator*() { return *ptr; }

  const T& operator*() const { return *ptr; }

  T* operator->() { return ptr; }

  int use_count() const { return block->shared_count; }

  ~SharedPtr() {
    if (block == nullptr)
      return;
    --(block->shared_count);
    if (!(block->shared_count)) {
      block->destroy();
      if (!(block->weak_count)) {
        block->free_memory();
      }
    }
  }
};

template <typename U, typename Allocator, typename... Args>
SharedPtr<U> allocateShared(Allocator alloc, Args&&... args) {
  using AllocTraits = typename std::template allocator_traits<Allocator>;
  using SharedAllocator = typename AllocTraits::template rebind_alloc<
      SharedControlBlock<U, Allocator>>;
  using SharedAllocatorTraits =
      typename std::template allocator_traits<SharedAllocator>;

  SharedAllocator sharedAlloc = alloc;
  SharedControlBlock<U, Allocator>* pt =
      SharedAllocatorTraits::allocate(sharedAlloc, 1);
  SharedAllocatorTraits::construct(sharedAlloc, pt, alloc,
                                   std::forward<Args>(args)...);

  BaseControlBlock* pointer = reinterpret_cast<BaseControlBlock*>(pt); 
  (pointer->shared_count)++;  
  SharedPtr<U> ptr = pt;
  return ptr;
}

template <typename U, typename... Args>
SharedPtr<U> makeShared(Args&&... args) {
  return allocateShared<U, std::allocator<U>, Args...>(
      std::allocator<U>(), std::forward<Args>(args)...);
}

template <typename T> class WeakPtr {
private:
  BaseControlBlock* block;  

  template <typename U> friend class WeakPtr;

  template <typename U> friend class SharedPtr;

public:
  int use_count() const {
    if (block == nullptr)
      return 0;
    return block->shared_count;
  }

  WeakPtr() : block(nullptr) {}

  template <typename U>
  WeakPtr(const SharedPtr<U>& other) : block(other.block) {
    if (block) {
      (block->weak_count)++;
    }
  }

  WeakPtr(const WeakPtr<T>& other) : block(other.block) {
    if (block) {
      (block->weak_count)++;
    }
  }

  template <typename U>
  WeakPtr(const WeakPtr<U>& other) : block(other.block) {
    if (block) {
      (block->weak_count)++;
    }
  }

  template <typename U>
  WeakPtr(WeakPtr<U>&& other) : block(other.block) {
    other.block = nullptr;    
  }

  WeakPtr<T>& operator=(const WeakPtr<T>& other) {
    WeakPtr<T> copy = other;
    swap(block, copy.block);    
    return *this;
  }

  template <typename U> WeakPtr<T>& operator=(WeakPtr<U>&& other) {
    WeakPtr<T> copy = other;

    other.block = nullptr;    

    swap(block, copy.block);    
    return *this;
  }

  bool expired() const {
    if (block == nullptr)
      return true;
    return !(block->shared_count);
  }

  SharedPtr<T> lock() const { return SharedPtr<T>(block, reinterpret_cast<T*>(block->get_ptr())); }

  ~WeakPtr() {
    if (block == nullptr)
      return;
    --(block->weak_count);
    if (!(block->shared_count) && !(block->weak_count)) {
      block->free_memory();
    }
  }
};
