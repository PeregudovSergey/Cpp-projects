#include <bits/stdc++.h>

typedef std::complex<double> comp;
typedef long long ll;
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

std::vector<int> multiply(const std::vector<short>& a,
                          const std::vector<short>& b) {
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

class BigInteger {
private:
  short ZERO = 0;
  const int base = 100;
  const int len_base = 2; 
  std::vector<short> digit;
  int sign;
  int siz;

  short operator[](int id) const {
    if (id < 0 || id >= siz)
      return ZERO;
    return digit[id];
  }

  short& operator[](int id) {
    if (id < 0 || id >= siz)
      return ZERO;
    return digit[id];
  }

  void power_base(int add_bits) {
    std::reverse(digit.begin(), digit.end());
    siz += add_bits;
    for (int i = 0; i < add_bits; ++i) {
      digit.push_back(0);
    }
    std::reverse(digit.begin(), digit.end());
    erase();
  }

  void erase() {
    while ((int)digit.size() > 1 && digit.back() == 0) {
      digit.pop_back();
      --siz;
    }
  }

public:
  bool is_null() const { return (siz == 1 && digit[0] == 0); }
  void clear() {
    digit.clear();
    sign = 1;
    siz = 0;
  }

  void resize(int n) {
    clear();
    sign = 1;
    siz = n;
    digit.resize(n);
  }

  BigInteger() {
    clear();
    sign = 1;
    digit = {0};
    siz = 1;
  };

  BigInteger(const std::vector<short> &d) {
    clear();
    sign = 1;
    digit = d;
    siz = d.size();
  };

  BigInteger(const std::vector<int> &d) {
    clear();
    sign = 1;
    digit.clear();
    for (auto x : d) {
      digit.push_back(x);
    }
    siz = d.size();
  };

  BigInteger(int x) {
    clear();
    if (x == 0) {
      sign = 1;
      siz = 1;
      digit = {0};
      return;
    }
    if (x > 0) {
      sign = 1;
    } else {
      x *= -1;
      sign = -1;
    }
    while (x) {
      digit.push_back(x % base);
      ++siz;
      x /= base;
    }
  };

  BigInteger(ll x) {
    clear();
    if (x == 0) {
      sign = 1;
      siz = 1;
      digit = {0};
      return;
    }
    if (x > 0) {
      sign = 1;
    } else {
      x *= -1;
      sign = -1;
    }
    while (x) {
      digit.push_back(x % base);
      ++siz;
      x /= base;
    }
  };

  BigInteger(__int128 x) {
    clear();
    if (x == 0) {
      sign = 1;
      siz = 1;
      digit = {0};
      return;
    }
    if (x > 0) {
      sign = 1;
    } else {
      x *= -1;
      sign = -1;
    }
    while (x) {
      digit.push_back(x % base);
      ++siz;
      x /= base;
    }
  };

  BigInteger(const std::string& s) {
    clear();

    int sz = (int)s.size();
    int start = 0;
    if (s[0] == '-') {
      sign = -1;
      start = 1;
    } else if (s[1] == '+') {
      sign = 1;
      start = 1;
    }
    for (int i = sz - 1; i >= start; i -= 2) {
      if (i == start) {
        digit.push_back(s[i] - '0');
      } else {
        digit.push_back((s[i - 1] - '0') * 10 + s[i] - '0');
      }
      ++siz;
    }
  };

  BigInteger operator-() const {
    BigInteger copy = (*this);
    if (is_null()) {
      return copy;
    }
    copy.sign *= -1;
    return copy;
  }

  friend bool operator<(const BigInteger& a, const BigInteger& b);
  friend bool operator<=(const BigInteger& a, const BigInteger& b);
  friend bool operator>(const BigInteger& a, const BigInteger& b);
  friend bool operator>=(const BigInteger& a, const BigInteger& b);
  friend bool operator==(const BigInteger& a, const BigInteger& b);
  friend bool operator!=(const BigInteger& a, const BigInteger& b);

  void mult_by_two() {
    for (int i = 0; i < (int)digit.size(); ++i) {
      digit[i] *= 2;
    }
    digit.push_back(0);
    ++siz;
    for (int i = 0; i < (int)digit.size() - 1; ++i) {
      digit[i + 1] += digit[i] / base;
      digit[i] %= base;
    }
    while ((int)digit.size() >= 2 && digit.back() == 0) {
      digit.pop_back();
      --siz;
    }
  }

  void divide_by_two() {
    for (int i = (int)digit.size() - 1; i >= 1; --i) {
      if (digit[i] % 2) {
        --digit[i];
        digit[i - 1] += base;
      }
    }
    for (int i = 0; i < (int)digit.size(); ++i) {
      digit[i] /= 2;
    }
    while ((int)digit.size() >= 2 && digit.back() == 0) {
      digit.pop_back();
      --siz;
    }
  }

  bool parity() const { return digit[0] % 2; }

  void swap(BigInteger& a, BigInteger& b) {
    std::swap(a.digit, b.digit);
    std::swap(a.sign, b.sign);
    std::swap(a.siz, b.siz);
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
    if (sign != value.sign) {
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

    if (siz > value.siz && sign == 1 && value.sign == 1) {
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

    for (int i = 0; i < a.siz; ++i) {
      if (a[i] >= b[i]) {
        a[i] -= b[i];
      } else {
        a[i] += base;
        a[i] -= b[i];
        a[i + 1] -= 1;
      }
    }

    if (sign == 1) {
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
    digit.push_back(0);
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
    std::vector<int> result = multiply(digit, value.digit);
    for (int x = 0; x < 1; ++x) {
      result.push_back(0);
    }
    for (int i = 0; i < (int)result.size() - 1; ++i) {
      result[i + 1] += result[i] / base;
      result[i] %= base;
    }
    BigInteger copy = result;
    copy.erase();
    copy.sign = sign * value.sign;
    swap(*this, copy);
    if (is_null()) {
      sign = 1;
    }
    return *this;
  }

  BigInteger& operator/=(const BigInteger& value) {
    if (siz <= 15 && (int)value.siz <= 15) {
      __int128 xx = 0;
      for (int i = (int)siz - 1; i >= 0; --i) {
        xx *= base;
        xx += digit[i];
      }
      xx *= sign;
      __int128 yy = 0;
      for (int i = (int)value.siz - 1; i >= 0; --i) {
        yy *= base;
        yy += value[i];
      }
      yy *= value.sign;
      *this = xx / yy;
      return *this;
    }
    if (is_null()) {
      return *this;
    }
    int new_sign = sign * value.sign;
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
      sign = new_sign;
      return *this;
    }

    xx = b;

    BigInteger pow = 1;
    int add_bits = max(0, (int)a.siz - xx.siz - 1);
    int it = add_bits * len_base;
    pow.power_base(add_bits);
    xx.power_base(add_bits);
    bool tet = false;
    while (xx <= a) {
      tet = true;
      ++it;
      xx.power_ten();
      pow.power_ten();
    }

    if (tet) {
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
    res.sign = new_sign;
    *this = res;
    return *this;
  }

  std::string toString() const {
    std::string res = "";
    if (sign == -1) {
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

  friend BigInteger gcd(const BigInteger& A, const BigInteger& B);
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
  if (a.sign != b.sign)
    return false;
  if (a.siz != b.siz)
    return false;
  return (a.digit == b.digit);
}

bool operator!=(const BigInteger& a, const BigInteger& b) { return !(a == b); }

bool operator<(const BigInteger& a, const BigInteger& b) {
  if (a.sign < b.sign)
    return true;
  if (a.sign > b.sign)
    return false;
  if (a.sign == -1) {
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
  return (!(a < b) && a != b);
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
  // std::cerr << "here:\n";
  if (a == 0) {
    return b;
  }
  if (b == 0) {
    return a;
  }

  // std::cerr << a << " " << b << '\n';
  BigInteger res = 1;
  while (a > 0 && b > 0) {
    // std::cerr << a << " " << b << '\n';
    if (a.parity() == 0 && b.parity() == 0) {
      res.mult_by_two();
      a.divide_by_two();
      b.divide_by_two();
    } else if (a.parity() == 0) {
      a.divide_by_two();
    } else if (b.parity() == 0) {
      b.divide_by_two();
    } else {
      if (a >= b)
        a -= b;
      else
        b -= a;
    }
  }
  return res * (a + b);
}

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
  friend bool operator<=(const Rational& a, const Rational& b);
  friend bool operator>(const Rational& a, const Rational& b);
  friend bool operator>=(const Rational& a, const Rational& b);
  friend bool operator==(const Rational& a, const Rational& b);
  friend bool operator!=(const Rational& a, const Rational& b);
  friend Rational operator+(const Rational& a, const Rational& b);
  friend Rational operator-(const Rational& a, const Rational& b);
  friend Rational operator*(const Rational& a, const Rational& b);
  friend Rational operator/(const Rational& a, const Rational& b);
  friend Rational operator%(const Rational& a, const Rational& b);

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
  friend std::istream& operator>>(std::istream& is, Rational& value);
};

std::ostream& operator<<(std::ostream& os, const Rational& x) {
  os << x.asDecimal(30);
  return os;
}

std::istream& operator>>(std::istream& is, Rational& x) {
  int xx;
  is >> xx;
  x = xx;
  return is;
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
  return (!(a < b) && a != b);
}

bool operator<=(const Rational& a, const Rational& b) {
  return (a < b) || (a == b);
}

bool operator>=(const Rational& a, const Rational& b) {
  return (a > b) || (a == b);
}

template <int X, int div, bool isLess> struct checkPrime {
  static const bool prime =
      X % div && checkPrime<X, div + 1, (div + 1) * (div + 1) <= X>::prime;
};

template <int X, int div> struct checkPrime<X, div, false> {
  static const bool prime = true;
};

template <int N> struct isPrime {
  static const bool prime = checkPrime<N, 2, 2 * 2 <= N>::prime;
};

template <> struct isPrime<0> { static const bool prime = false; };

template <> struct isPrime<1> { static const bool prime = false; };

template <size_t N> class Residue;

template <size_t N>
Residue<N> operator+(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator-(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator*(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator/(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator==(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator<(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator<=(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator>(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N>
Residue<N> operator>=(const Residue<N> &lhs, const Residue<N> &rhs);

template <size_t N> std::istream& operator>>(std::istream& is, Residue<N> &a);

template <size_t N>
std::ostream& operator<<(std::ostream& os, const Residue<N> &a);

template <size_t N> class Residue {
private:
  int x;

  int power(int a, int pw) const {
    if (pw == 0)
      return 1;
    ll t = power(a, pw / 2);
    t *= t;
    t %= (int)N;
    if (pw & 1) {
      t *= a;
      t %= (int)N;
    }
    return t;
  }

  int inv(int x) const { return power(x, (int)N - 2); }

public:
  Residue() : x(0) {}

  Residue(int x) : x((x % (int)N + N) % N) {}

  Residue(const Residue<N> &value) : x(value.x) {}

  void mod() {
    if (x >= N) {
      x %= (int)N;
      return;
    }
    if (x < 0) {
      x %= (int)N;
      x += N;
      return;
    }
  }

  Residue<N> &operator=(const Residue<N> &value) {
    x = value.x;
    return *this;
  }

  Residue<N> &operator+=(const Residue<N> &value) {
    x += value.x;
    x %= (int)N;
    if (x < 0)
      x += N;
    return *this;
  }

  Residue<N> &operator-=(const Residue<N> &value) {
    x -= value.x;
    x %= (int)N;
    if (x < 0)
      x += N;
    return *this;
  }

  Residue<N> &operator*=(const Residue<N> &value) {
    ll pt = x;
    pt *= value.x;
    pt %= (int)N;

    x = pt;
    return *this;
  }

  Residue<N> &operator/=(const Residue<N> &value) {
    static_assert(isPrime<N>::prime, "Stop!\n");
    ll pt = x;
    pt *= power(value.x, N - 2);
    pt %= (int)N;
    x = pt;
    return *this;
  }

  operator int() const { return x; }

  Residue& operator++() {
    *this += 1;
    return *this;
  }

  Residue& operator--() {
    (*this) -= 1;
    return *this;
  }

  Residue operator--(int) {
    BigInteger copy = *this;
    (*this) -= 1;
    return copy;
  }

  Residue operator++(int) {
    Residue copy = *this;
    (*this) += 1;
    return copy;
  }
};

template <size_t N>
Residue<N> operator+(const Residue<N> &lhs, const Residue<N> &rhs) {
  Residue<N> copy = lhs;
  copy += rhs;
  return copy;
}

template <size_t N>
Residue<N> operator-(const Residue<N> &lhs, const Residue<N> &rhs) {
  Residue<N> copy = lhs;
  copy -= rhs;
  return copy;
}

template <size_t N>
Residue<N> operator*(const Residue<N> &lhs, const Residue<N> &rhs) {
  Residue<N> copy = lhs;
  copy *= rhs;
  return copy;
}

template <size_t N>
Residue<N> operator/(const Residue<N> &lhs, const Residue<N> &rhs) {
  Residue<N> copy = lhs;
  copy /= rhs;
  return copy;
}

template <size_t N>
Residue<N> operator==(const Residue<N> &lhs, const Residue<N> &rhs) {
  return lhs.x == rhs.x;
}

template <size_t N>
Residue<N> operator<(const Residue<N> &lhs, const Residue<N> &rhs) {
  return lhs.x < rhs.x;
}

template <size_t N>
Residue<N> operator>(const Residue<N> &lhs, const Residue<N> &rhs) {
  return lhs.x > rhs.x;
}

template <size_t N>
Residue<N> operator<=(const Residue<N> &lhs, const Residue<N> &rhs) {
  return lhs.x <= rhs.x;
}

template <size_t N>
Residue<N> operator>=(const Residue<N> &lhs, const Residue<N> &rhs) {
  return lhs.x >= rhs.x;
}

template <size_t N> std::istream& operator>>(std::istream& is, Residue<N> &a) {
  int x;
  is >> x;
  a = x;
  return is;
}

template <size_t N>
std::ostream& operator<<(std::ostream& os, const Residue<N> &a) {
  os << int(a);
  return os;
}

template <size_t N, size_t M, typename Field> class Matrix;

template <size_t N, size_t M, size_t K, typename Field>
Matrix<N, K, Field> operator*(const Matrix<N, M, Field> &lhs,
                              const Matrix<M, K, Field> &rhs);

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator*(const Matrix<N, M, Field> &lhs, const Field& x);

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator*(const Field& x, const Matrix<N, M, Field> &rhs);

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator+(const Matrix<N, M, Field> &lhs,
                              const Matrix<N, M, Field> &rhs);

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator-(const Matrix<N, M, Field> &lhs,
                              const Matrix<N, M, Field> &rhs);

template <size_t N, size_t M, typename Field>
bool operator==(const Matrix<N, M, Field> &lhs, const Matrix<N, M, Field> &rhs);

template <size_t N, size_t M, typename Field>
bool operator!=(const Matrix<N, M, Field> &lhs, const Matrix<N, M, Field> &rhs);

template <size_t N, size_t M, typename Field = Rational> class Matrix {
private:
  std::vector<std::vector<Field>> ms =
      std::vector<std::vector<Field>>(N, std::vector<Field>(M, Field(0)));

  void swap(std::vector<Field> &a, std::vector<Field> &b) {
    std::vector<Field> c = a;
    a = b;
    b = c;
  }

  std::vector<Field> make_zero(size_t i1, size_t i2, size_t j) const {
    Field k = ms[i2][j] / ms[i1][j];
    std::vector<Field> a = ms[i1];
    std::vector<Field> b = ms[i2];
    for (int i = 0; i < (int)M; ++i) {
      b[i] -= a[i] * k;
    }
    return b;
  }

  std::pair<Matrix<N, M, Field>, int> gauss() const {
    // std::cerr << "gauss:\n";
    Matrix stepped = ms;
    int swap = 0;
    size_t lst = 0;
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = lst; j < N; ++j) {
        if (stepped[j][i] != 0) {
          std::swap(stepped[j], stepped[lst]);
          for (int j = lst + 1; j < (int)N; ++j) {
            stepped[j] = stepped.make_zero(lst, j, i);
          }
          if (j != lst) {
            swap ^= 1;
          }
          ++lst;
          break;
        }
      }
    }
    return {stepped, swap};
  }

public:
  std::vector<Field>& operator[](size_t i) {    
    return ms[i];
  }

  std::vector<Field> operator[](size_t i) const {    
    return ms[i];
  }
  Matrix() {
    if (N == M) {
      for (size_t i = 0; i < N; ++i) {
        ms[i][i] = 1;
      }
    }
  }

  void clear() {
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] = 0;
      }
    }
  }

  template <typename T> Matrix(const std::vector<std::vector<T>>& build) {    
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] = build[i][j];
      }
    }
  }

  template <typename T>
  Matrix(const std::initializer_list<std::initializer_list<T>>& build) {    
    int i = 0;
    for (auto p : build) {
      int j = 0;
      for (auto p1 : p) {
        ms[i][j++] = p1;
      }
      ++i;
    }
  }

  Matrix(const std::vector<std::vector<int> >& build) {    
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] = build[i][j];
      }
    }
  }

  Matrix(const Matrix<N, M, Field>& value) {
    // std::cerr << N << " " << M << '\n';
    // std::cerr << "construct MATRIX:\n";
    // std::cerr << value << '\n';
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] = value[i][j];
      }
    }
    // std::cerr << "end\n";
    // std::cerr << "finished\n";
  }

  std::vector<Field> getRow(size_t i) const {
    // std::cerr << "get row:\n";
    return ms[i];
  }

  std::vector<Field> getColumn(size_t j) const {
    // std::cerr << "get col:\n";
    std::vector<Field> col(N);
    for (size_t i = 0; i < N; ++i) {
      col[i] = ms[i][j];
    }
    return col;
  }

  Matrix<N, M, Field>& operator=(const Matrix<N, M, Field>& rhs) {    
    ms = rhs.ms;
    return *this;
  }

  Matrix<N, M, Field>& operator+=(const Matrix<N, M, Field>& add) {    
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] += add[i][j];
      }
    }
    return *this;
  }

  Matrix<N, M, Field> &operator-=(const Matrix<N, M, Field>& add) {    
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] -= add[i][j];
      }
    }
    return *this;
  }

  Matrix<N, M, Field> &operator*=(const Field& f) {    
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        ms[i][j] *= f;
      }
    }
    std::cerr << "end\n";
    return *this;
  }

  Matrix<N, N, Field> &operator*=(const Matrix<N, N, Field>& rhs) {    
    static_assert(N == M, "Wrong size in mult!\n");
    Matrix<N, N, Field> result;
    result.clear();    
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < N; ++j) {
        for (size_t k = 0; k < N; ++k) {
          result[i][k] += ms[i][j] * rhs[j][k];
        }
      }
    }
    ms = result.ms;    
    return *this;
  }

  Field det() const {    
    static_assert(N == M, "Wrong size in det!\n");
    std::pair<Matrix<N, N, Field>, int> result = gauss();
    Matrix<N, N, Field> stepped = result.first;
    int swap = result.second;
    Field det = 1;
    for (size_t i = 0; i < N; ++i) {
      det *= stepped[i][i];
    }
    if (swap) {
      det *= (-1);
    }    
    return det;
  }

  Matrix<M, N, Field> transposed() const {    
    Matrix<M, N, Field> new_matrix;
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        new_matrix[j][i] = ms[i][j];
      }
    }
    return new_matrix;
  }

  int rank() const {    
    Matrix<N, M, Field> a = gauss().first;
    int rk = 0;
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        if (a[i][j] != 0) {
          ++rk;
          break;
        }
      }
    }    
    return rk;
  }

  Matrix<N, N, Field>& invert() {        
    static_assert(N == M, "Wrong size in det!\n");
    Matrix<N, N, Field> inv;
    for (size_t i = 0; i < N; ++i) {      
      for (size_t j = i; j < N; ++j) {
        if (ms[j][i] != 0) {
          std::swap(ms[j], ms[i]);
          std::swap(inv[j], inv[i]);

          Field k = ms[i][i];
          for (size_t id = 0; id < N; ++id) {
            ms[i][id] /= k;
            inv[i][id] /= k;
          }
          for (size_t j = 0; j < N; ++j) {
            if (j == i)
              continue;
            Field k = ms[j][i];
            for (size_t id = 0; id < N; ++id) {
              ms[j][id] -= ms[i][id] * k;
              inv[j][id] -= inv[i][id] * k;
            }
          }
          break;
        }
      }
    }
    *this = inv;    
    return *this;
  }

  Matrix<N, N, Field> inverted() const {    
    static_assert(N == M, "Wrong size in det!\n");
    Matrix<N, N, Field> inv = *this;
    inv.invert();
    return inv;
  }

  Field trace() const {    
    static_assert(N == M, "Wrong size to calculate Trace!\n");
    Field res = 0;
    for (size_t i = 0; i < N; ++i) {
      res += ms[i][i];
    }    
    return res;
  }

  friend std::ostream& operator<<(std::ostream& os,
                                  const Matrix<N, M, Field>& a) {
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        os << a[i][j] << ' ';
      }
      os << '\n';
    }
    return os;
  }

  friend std::istream& operator>>(std::istream& is, Matrix<N, M, Field>& a) {
    for (size_t i = 0; i < N; ++i) {
      for (size_t j = 0; j < M; ++j) {
        is >> a[i][j];
      }
    }
    return is;
  }
};

template <size_t N, size_t M, size_t K, typename Field>
Matrix<N, K, Field> operator*(const Matrix<N, M, Field>& lhs,
                              const Matrix<M, K, Field>& rhs) {  
  Matrix<N, K, Field> result;
  result.clear();
  for (size_t i = 0; i < N; ++i) {
    for (size_t j = 0; j < M; ++j) {
      for (size_t k = 0; k < K; ++k) {
        result[i][k] += lhs[i][j] * rhs[j][k];
      }
    }
  }  
  return result;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator+(const Matrix<N, M, Field>& lhs,
                              const Matrix<N, M, Field>& rhs) {
  Matrix<N, M, Field> result = lhs;
  result += rhs;
  return result;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator-(const Matrix<N, M, Field>& lhs,
                              const Matrix<N, M, Field>& rhs) {
  Matrix<N, M, Field> result = lhs;
  result -= rhs;
  return result;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator*(const Matrix<N, M, Field>& lhs, const Field& x) {
  Matrix<N, M, Field> copy = lhs;
  copy *= x;
  return copy;
}

template <size_t N, size_t M, typename Field>
Matrix<N, M, Field> operator*(const Field& x, const Matrix<N, M, Field>& rhs) {
  Matrix<N, M, Field> copy = rhs;
  copy *= x;
  return copy;
}

template <size_t N, size_t M, typename Field>
bool operator==(const Matrix<N, M, Field>& lhs,
                const Matrix<N, M, Field>& rhs) {
  for (size_t i = 0; i < N; ++i) {
    for (size_t j = 0; j < M; ++j) {
      if (lhs[i][j] != rhs[i][j])
        return false;
    }
  }
  return true;
}

template <size_t N, size_t M, typename Field>
bool operator!=(const Matrix<N, M, Field>& lhs,
                const Matrix<N, M, Field>& rhs) {
  return !(lhs == rhs);
}

template <size_t N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;
