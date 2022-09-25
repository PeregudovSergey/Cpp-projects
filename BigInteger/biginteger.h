#include <bits/stdc++.h>

typedef std::complex<double> comp;

const double PI = acos(-1);

void fft(std::vector<comp>& a, bool invert) {
  int n = (int)a.size();

  for (int i = 1, j = 0; i < n; ++i) {
    int bit = n >> 1;
    for (; j >= bit; bit >>= 1)
      j -= bit;
    j += bit;
    if (i < j)
      swap(a[i], a[j]);
  }

  for (int len = 2; len <= n; len <<= 1) {
    double ang = 2 * PI / len * (invert ? -1 : 1);
    comp wlen(cos(ang), sin(ang));
    for (int i = 0; i < n; i += len) {
      comp w(1);
      for (int j = 0; j < len / 2; ++j) {
        comp u = a[i + j], v = a[i + j + len / 2] * w;
        a[i + j] = u + v;
        a[i + j + len / 2] = u - v;
        w *= wlen;
      }
    }
  }
  if (invert)
    for (int i = 0; i < n; ++i)
      a[i] /= n;
}

std::vector<int> multiply(const std::vector<short> &a,
                          const std::vector<short> &b) {
  std::vector<comp> fa(a.begin(), a.end()), fb(b.begin(), b.end());
  size_t n = 1;
  while (n < std::max(a.size(), b.size()))
    n <<= 1;
  n <<= 1;
  fa.resize(n), fb.resize(n);

  fft(fa, false), fft(fb, false);
  for (size_t i = 0; i < n; ++i)
    fa[i] *= fb[i];
  fft(fa, true);
  std::vector<int> res(n);
  for (size_t i = 0; i < n; ++i)
    res[i] = (int)(fa[i].real() + 0.5);
  return res;
}

int max(int a, int b) { return std::max(a, b); }

class BigInteger;
BigInteger operator+(const BigInteger& a, const BigInteger& b);
BigInteger operator-(const BigInteger& a, const BigInteger& b);
BigInteger operator*(const BigInteger& a, const BigInteger& b);
BigInteger operator/(const BigInteger& a, const BigInteger& b);
BigInteger operator%(const BigInteger& a, const BigInteger& b);
bool operator<=(const BigInteger& a, const BigInteger& b);
bool operator>(const BigInteger& a, const BigInteger& b);
bool operator>=(const BigInteger& a, const BigInteger& b);
bool operator!=(const BigInteger& a, const BigInteger& b);


class BigInteger {
private:
  std::vector<short> digit;
  int isPositive;
  int siz;  
  const int base = 100;
  const int len_base = 2;

  short operator[](int id) const {
    if (id < 0 || id >= siz)
      return 0;
    return digit[id];
  }

  short& operator[](int id) {    
    return digit[id];
  }

  void power_base(int add_bits) {
    std::reverse(digit.begin(), digit.end());

    siz += add_bits;
    digit.resize(siz); 

    for (int i = siz - add_bits; i < siz; ++i) {
      digit[i] = 0; 
    }
    std::reverse(digit.begin(), digit.end());
    erase();
  }

  void erase() {
    while (digit.size() > 1u && digit.back() == 0) {
      digit.pop_back();
      --siz;
    }
  }

  void swap(BigInteger& a, BigInteger& b) {
    std::swap(a.digit, b.digit);
    std::swap(a.isPositive, b.isPositive);
    std::swap(a.siz, b.siz);
  }

  void clear() {
    digit.clear();
    isPositive = 1;
    siz = 0;
  }

public:
  bool is_null() const { return (siz == 1 && digit[0] == 0); }  

  void resize(int n) {
    clear();
    isPositive = 1;
    siz = n;
    digit.resize(n);
  }

  BigInteger() {
    clear();
    isPositive = 1;
    digit = {0};
    siz = 1;
  }

  BigInteger(const std::vector<short>& d) {
    clear();
    isPositive = 1;
    digit = d;
    siz = d.size();
  }

  template<typename T> 
  BigInteger(const std::vector<T>& d) {
    clear();
    isPositive = 1;    
    digit.resize(d.size()); 
    for (size_t i = 0; i < d.size(); ++i) {
      digit[i] = d[i]; 
    }    
    siz = d.size();
  }

  BigInteger(int x) {
    clear();
    if (x == 0) {
      isPositive = 1;
      siz = 1;
      digit = {0};
      return;
    }
    if (x > 0) {
      isPositive = 1;
    } else {
      x *= -1;
      isPositive = -1;
    }
    while (x) {
      digit.push_back(x % base);
      ++siz;
      x /= base;
    }
  }

  BigInteger(const std::string& s) {
    clear();

    int sz = s.size();
    int start = 0;

    if (s[0] == '-') {
      isPositive = -1;
      start = 1;
    } else if (s[1] == '+') {
      isPositive = 1;
      start = 1;
    }
    
    digit.reserve((sz - start) / len_base + 1); 
    siz = 0; 

    for (int i = sz - 1; i >= start; i -= len_base) {
      short xx = 0; 
      for (int j = std::max(start, i - len_base + 1); j <= i; ++j) {        
        xx *= 10; 
        xx += (s[j] - '0'); 
      }      
      digit.push_back(xx); 
      ++siz;     
    }    

    erase(); 
  }

  BigInteger operator-() const {
    BigInteger copy = (*this);
    if (is_null()) {
      return copy;
    }
    copy.isPositive *= -1;
    return copy;
  }    

  BigInteger abs() const {
    BigInteger copy = *this;
    if (copy > 0)
      return copy;
    copy = -copy;
    return copy;
  }

  BigInteger& operator=(const BigInteger& value) {
    BigInteger copy = value;
    swap(*this, copy);
    return *this;
  }

  BigInteger& operator-=(const BigInteger& value) {
    if (isPositive != value.isPositive) {
      int sz = max(siz, value.siz) + 1;
      digit.resize(sz);
      siz = sz;

      int add = 0;
      for (int i = 0; i < sz; ++i) {
        int cc = digit[i] + value[i] + add;
        digit[i] = cc % base;
        add = cc / base;
        if (add == 0 && i >= value.siz)
          break;
      }
      erase();
      return *this;
    }

    if (siz > value.siz && isPositive == 1 && value.isPositive == 1) {
      for (int i = 0; i < siz; ++i) {
        if (digit[i] >= 0 && value[i] == 0 && i > value.siz) {
          break;
        }
        if (digit[i] >= value[i]) {
          digit[i] -= value[i];
        } else {
          digit[i] = (digit[i] + base) - value[i];
          digit[i + 1] -= 1;
        }
      }
      erase();
      return *this;
    }

    BigInteger a = (*this).abs();
    BigInteger b = value.abs();

    bool change_sign = false;
    if (a < b) {
      swap(a, b);
      change_sign = true;
    }

    const BigInteger bb = b; 

    for (int i = 0; i < a.siz; ++i) {
      if (a[i] >= bb[i]) {
        a[i] -= bb[i];
      } else {
        a[i] += base;
        a[i] -= bb[i];
        a[i + 1] -= 1;
      }
    }

    if (isPositive == 1) {
      if (change_sign) {
        a = -a;
      }
    } else {
      if (!change_sign) {
        a = -a;
      }
    }
    a.erase();
    *this = a;
    return *this;
  }

  void power_ten() {
    for (int i = 0; i < siz; ++i) {
      digit[i] *= 10;
    }
    digit.resize(siz + 1);     
    for (int i = 0; i < siz; ++i) {
      digit[i + 1] += digit[i] / base;
      digit[i] %= base;
    }
    ++siz;
    erase();
  }

  void div_ten() {
    for (int i = siz - 1; i >= 0; --i) {
      if (digit[i] % 10 == 0) {
        digit[i] /= 10;
      } else {
        digit[i - 1] += (digit[i] % 10) * base;
        digit[i] -= digit[i] % 10;
        digit[i] /= 10;
      }
    }
    erase();
  }

  BigInteger& operator+=(const BigInteger& value) {
    (*this) -= (-value);
    return *this;
  }

  BigInteger& operator++() {
    *this += 1;
    return *this;
  }

  BigInteger& operator--() {
    (*this) -= 1;
    return *this;
  }

  BigInteger operator--(int) {
    BigInteger copy = *this;
    (*this) -= 1;
    return copy;
  }

  BigInteger operator++(int) {
    BigInteger copy = *this;
    (*this) += 1;
    return copy;
  }

  BigInteger& operator*=(const BigInteger& value) {
    std::vector<int> result;
    result = multiply(digit, value.digit);    
    for (int x = 0; x < 1; ++x) {
      result.push_back(0);
    }
    for (int i = 0; i < (int)result.size() - 1; ++i) {
      result[i + 1] += result[i] / base;
      result[i] %= base;
    }
    BigInteger copy = result;
    copy.erase();
    copy.isPositive = isPositive * value.isPositive;
    swap(*this, copy);
    if (is_null()) {
      isPositive = 1;
    }
    return *this;
  }

  BigInteger& operator/=(const BigInteger& value) {
    if (is_null()) {
      return *this;
    }
    int new_sign = isPositive * value.isPositive;
    BigInteger a = (*this).abs();
    BigInteger b = value.abs();
    if (a < b) {
      *this = 0;
      return *this;
    }

    BigInteger xx = b;
    int need_to_power = (int)siz - (int)value.siz + 1;
    xx.power_base(need_to_power);
    BigInteger res = 1;
    res.power_base(need_to_power);

    a = xx - a;
    if (a == 0) {
      *this = res;
      isPositive = new_sign;
      return *this;
    }

    xx = b;

    BigInteger pow = 1;
    int add_bits = max(0, (int)a.siz - xx.siz - 1);
    int it = add_bits * len_base;
    pow.power_base(add_bits);
    xx.power_base(add_bits);

    bool was = false;
    while (xx <= a) {
      was = true;
      ++it;
      xx.power_ten();
      pow.power_ten();
    }

    if (was) {
      --it;
      xx.div_ten();
      pow.div_ten();
    }

    while (it >= 0) {
      for (int i = 1; i < 10; ++i) {
        if (a >= xx) {
          a -= xx;
          res -= pow;
        } else {
          break;
        }
      }
      if (it) {
        pow.div_ten();
        xx.div_ten();
      }
      --it;
    }
    if (a > 0) {
      --res;
    }
    res.isPositive = new_sign;
    *this = res;
    return *this;
  }

  std::string toString() const {
    std::string res = "";
    if (isPositive == -1) {
      res = "-";
    }
    for (int i = siz - 1; i >= 0; --i) {
      if (digit[i] < 10) {
        if (i != siz - 1) {
          res += "0";
        }
        res += std::to_string(digit[i]);
      } else {
        res += std::to_string(digit[i]);
      }
    }
    return res;
  }

  BigInteger& operator%=(const BigInteger& value) {
    BigInteger copy = *this;
    copy -= copy / value * value;
    *this = copy;
    return *this;
  }

  explicit operator bool() const { return !is_null(); }

  friend std::ostream& operator<<(std::ostream& os, const BigInteger& value);
  friend std::istream& operator>>(std::istream& is, BigInteger& x);
  
  friend bool operator<(const BigInteger& a, const BigInteger& b);
  friend bool operator==(const BigInteger& a, const BigInteger& b);
};

std::ostream& operator<<(std::ostream& os, const BigInteger& x) {
  os << x.toString();
  return os;
}

std::istream& operator>>(std::istream& is, BigInteger& x) {

  x.clear();

  is >> std::ws;

  std::string s = "";
  while (true) {
    char c1 = is.peek();
    if (c1 == ' ' || c1 == '\n' || c1 == EOF)
      break;
    char c;
    is.get(c);
    s += c;
  }
  x = s;

  return is;
}

bool operator==(const BigInteger& a, const BigInteger& b) {
  if (a.isPositive != b.isPositive)
    return false;
  if (a.siz != b.siz)
    return false;
  return (a.digit == b.digit);
}

bool operator!=(const BigInteger& a, const BigInteger& b) { return !(a == b); }

bool operator<(const BigInteger& a, const BigInteger& b) {
  if (a.isPositive < b.isPositive)
    return true;
  if (a.isPositive > b.isPositive)
    return false;
  if (a.isPositive == -1) {
    if (a.siz > b.siz)
      return true;
    if (a.siz < b.siz)
      return false;
    for (int i = a.siz - 1; i >= 0; --i) {
      if (a[i] > b[i])
        return true;
      if (a[i] < b[i])
        return false;
    }
    return false;
  } else {
    if (a.siz < b.siz)
      return true;
    if (a.siz > b.siz)
      return false;
    for (int i = a.siz - 1; i >= 0; --i) {
      if (a[i] < b[i])
        return true;
      if (a[i] > b[i])
        return false;
    }
    return false;
  }
}

bool operator>(const BigInteger& a, const BigInteger& b) {
  return b < a; 
}

bool operator<=(const BigInteger& a, const BigInteger& b) {
  return (a < b) || (a == b);
}

bool operator>=(const BigInteger& a, const BigInteger& b) {
  return (a > b) || (a == b);
}

BigInteger operator+(const BigInteger& a, const BigInteger& b) {
  BigInteger copy = a;
  copy += b;
  return copy;
}

BigInteger operator-(const BigInteger& a, const BigInteger& b) {
  BigInteger copy = a;
  copy -= b;
  return copy;
}

BigInteger operator*(const BigInteger& a, const BigInteger& b) {
  BigInteger copy = a;
  copy *= b;
  return copy;
}

BigInteger operator/(const BigInteger& a, const BigInteger& b) {
  BigInteger copy = a;
  copy /= b;
  return copy;
}

BigInteger operator%(const BigInteger& a, const BigInteger& b) {
  BigInteger copy = a;
  copy %= b;
  return copy;
}

BigInteger gcd(const BigInteger& A, const BigInteger& B) {
  BigInteger a = A.abs();
  BigInteger b = B.abs();    
  if (a == 0 && b == 0) {
    return 0;
  }
  while (a > 0 && b > 0) {
    if (a > b) {
      a %= b;
    } else {
      b %= a;
    }
  }
  return a + b;
}

class Rational; 
Rational operator+(const Rational& a, const Rational& b);
Rational operator-(const Rational& a, const Rational& b);
Rational operator*(const Rational& a, const Rational& b);
Rational operator/(const Rational& a, const Rational& b);
Rational operator%(const Rational& a, const Rational& b);
bool operator<=(const Rational& a, const Rational& b);
bool operator>(const Rational& a, const Rational& b);
bool operator>=(const Rational& a, const Rational& b);
bool operator!=(const Rational& a, const Rational& b);  

class Rational {
private:
  BigInteger p, q;
  void normal(BigInteger& a, BigInteger& b) {
    if (b < 0) {
      a = -a;
      b = -b;
    }
    BigInteger GCD = gcd(a, b);
    a /= GCD;
    b /= GCD;
  }

public:
  void norm() {
    BigInteger GCD = gcd(p, q);
    if (GCD != 0) {
      p /= GCD;
      q /= GCD;
    }
    if (q < 0) {
      q = -q;
      p = -p;
    }
  }

  Rational(const BigInteger& value) {
    p = value;
    q = 1;
  };

  Rational() {
    p = 0;
    q = 1;
  };

  Rational(int x) {
    p = x;
    q = 1;
  };

  Rational& operator-=(const Rational& value) {
    // std::cerr << "-=\n";
    p *= value.q;
    p -= value.p * q;
    q *= value.q;
    norm();
    return *this;
  }

  Rational& operator+=(const Rational& value) {
    p *= value.q;
    p += value.p * q;
    q *= value.q;
    norm();
    return *this;
  }

  Rational& operator*=(const Rational& value) {
    p *= value.p;
    q *= value.q;
    norm();
    return *this;
  }

  Rational& operator/=(const Rational& value) {
    p *= value.q;
    q *= value.p;
    norm();
    return *this;
  }

  friend bool operator<(const Rational& a, const Rational& b);    
  friend bool operator==(const Rational& a, const Rational& b);  

  Rational operator-() const {
    Rational copy = *this;
    copy.p = -copy.p;
    return copy;
  }

  std::string toString() const {
    std::string res = "";
    res += p.toString();
    if (q != 1) {
      res += "/";
      res += q.toString();
    }
    return res;
  }

  std::string asDecimal(size_t precision = 0) const {
    std::string res = "";
    BigInteger a = p;
    BigInteger b = q;
    BigInteger x = a / b;
    if (x == 0 && a < 0) {
      res += "-";
    }
    res += (x).toString();
    if (a < 0)
      a = -a;
    a %= b;
    if (precision == 0)
      return res;
    res += ".";
    for (size_t it = 0; it < precision; ++it) {
      a *= 10;
      res += (a / b).toString();
      a %= b;
    }
    return res;
  }

  explicit operator double() const {
    std::string res = asDecimal(100);
    return std::stod(res);
  }

  friend std::ostream& operator<<(std::ostream& os, const Rational& value);
};

std::ostream& operator<<(std::ostream& os, const Rational& x) {
  os << x.asDecimal(30);
  return os;
}

Rational operator+(const Rational& a, const Rational& b) {
  Rational copy = a;
  copy += b;
  return copy;
}

Rational operator-(const Rational& a, const Rational& b) {
  Rational copy = a;
  copy -= b;
  return copy;
}

Rational operator*(const Rational& a, const Rational& b) {
  Rational copy = a;
  copy *= b;
  return copy;
}

Rational operator/(const Rational& a, const Rational& b) {
  Rational copy = a;
  copy /= b;
  return copy;
}

bool operator==(const Rational& a, const Rational& b) {
  if (a.p * b.q == b.p * a.q)
    return true;
  return false;
}

bool operator!=(const Rational& a, const Rational& b) { return !(a == b); }

bool operator<(const Rational& a, const Rational& b) {
  if (a.p * b.q < b.p * a.q)
    return true;
  return false;
}

bool operator>(const Rational& a, const Rational& b) {
  return b < a; 
}

bool operator<=(const Rational& a, const Rational& b) {
  return (a < b) || (a == b);
}

bool operator>=(const Rational& a, const Rational& b) {
  return (a > b) || (a == b);
}
