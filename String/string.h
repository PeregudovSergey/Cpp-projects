#include <cstring>
#include <iostream>

class String;
String operator+(const String& lhs, const String& rhs); 

class String {
 private:
  char* str_;
  size_t sz;
  size_t cap = 0;

  bool check_pattern(int i, const String& str) const {    
    if (i + str.sz - 1 >= sz) return false;

    for (size_t j = 0; j < str.sz; ++j) {
      if (str_[i + j] != str.str_[j]) {
        return false; 
      }
    }
    return true; 
  }

  int find_pattern(size_t l, size_t r, int step, const String& str) const {
    while (l != r) {
      if (check_pattern(l, str)) {
        return l; 
      }
      l += step; 
    }
    return (check_pattern(l, str) ? l : sz); 
  }

  void swap(String& str) {
    std::swap(str_, str.str_);
    std::swap(sz, str.sz);
    std::swap(cap, str.cap);
  }  

 public:
  String() : str_(new char[1]), sz(0), cap(1) {}

  String(char c) : str_(new char(c)), sz(1), cap(1) {}

  String(const String& str) : str_(new char[str.sz]), sz(str.sz), cap(str.sz) {
    strncpy(str_, str.str_, sz);
  }

  String(const char* str) : str_(new char[strlen(str)]), sz(strlen(str)), cap(strlen(str)) { 
    strncpy(str_, str, sz);
  }
  
  String(size_t n, char c) : str_(new char[n]), sz(n), cap(n) {
    memset(str_, c, n);    
  }

  ~String() { delete[] str_; }

  char& operator[](size_t id) { return *(str_ + id); }

  char operator[](size_t id) const { return *(str_ + id); }  

  String& operator=(const String& str) {
    String copy(str);
    swap(copy);
    return *this;
  }

  size_t length() const { return sz; }

  void push_back(char c) {
    if (cap == sz) {
      String copy = *this;
      delete[] str_;
      cap *= 2;
      ++cap;
      str_ = new char[cap];
      strncpy(str_, copy.str_, sz);
    }
    str_[sz++] = c;
  }

  void pop_back() { str_[--sz] = '\0'; }

  char& front() { return str_[0]; }

  char& back() { return str_[sz - 1]; }

  char back() const { return str_[sz - 1]; }

  char front() const { return str_[0]; }  

  String& operator+=(const String& str) {    
    if (cap >= sz + str.sz) {      
      memcpy(str_ + sz, str.str_, str.sz); 
      sz += str.sz;       
      return *this; 
    }   

    while (sz + str.sz > cap) {
      cap *= 2; 
    }
    char* new_str = new char[sz + str.sz];     
    memcpy(new_str, str_, sz);     
    memcpy(new_str + sz, str.str_, str.sz);         
    sz += str.sz;     

    delete[] str_; 

    str_ = new char[cap]; 
    memcpy(str_, new_str, sz + str.sz);         

    delete[] new_str;  

    return *this; 
  }  

  size_t find(const String& str) const {
    if (sz < str.sz) return length(); 
    return find_pattern(0, sz - str.sz, 1, str);     
  }
  

  size_t rfind(const String& str) const {
    if (sz < str.sz) return length(); 
    return find_pattern(sz - str.sz, 0, -1, str);     
  }

  String substr(size_t start, size_t count) const {
    String sub(count, '\0');
    memcpy(sub.str_, str_ + start, count);     
    return sub;
  }

  bool empty() const { return length() == 0; }

  void clear() {
    delete[] str_;
    str_ = nullptr;
    sz = 0;
    cap = 0;
  }  

  friend std::ostream& operator<<(std::ostream& os, const String& str);
  friend bool operator==(const String& lhs, const String& rhs);
};

String operator+(const String& lhs, const String& rhs) {
  String copy = lhs; 
  copy += rhs; 
  return copy; 
}

bool operator==(const String& lhs, const String& rhs) {
  if (lhs.sz != rhs.sz) return false;
  for (size_t i = 0; i < lhs.sz; ++i) {
    if (lhs[i] != rhs[i]) {
      return false;
    }
  }
  return true;
};

std::ostream& operator<<(std::ostream& os, const String& x) {
  for (size_t i = 0; i < x.length(); ++i) {
    os << x[i];
  }
  return os;
}

std::istream& operator>>(std::istream& is, String& x) {
  x.clear();

  is >> std::ws;

  while (true) {
    char c1 = is.peek();
    if (c1 == ' ' || c1 == '\n' || c1 == EOF) break;
    char c;
    is.get(c);
    x.push_back(c);
  }

  return is;
}
