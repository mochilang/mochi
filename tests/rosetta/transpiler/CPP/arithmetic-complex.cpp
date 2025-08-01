// Generated by Mochi transpiler v0.10.40 on 2025-07-25 21:22:11 GMT+7
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <vector>
#include <cstdlib>
#include <chrono>
#include <sys/resource.h>


static int _now() {
    static long long seed = 0;
    static bool seeded = false;
    if (!seeded) {
        const char* s = std::getenv("MOCHI_NOW_SEED");
        if (s && *s) { seed = std::atoll(s); seeded = true; }
    }
    if (seeded) {
        seed = (seed * 1664525 + 1013904223) % 2147483647;
        return static_cast<int>(seed);
    }
    return (int)(std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count() % 2147483647);
}
static long _mem() {
    struct rusage usage{};
    getrusage(RUSAGE_SELF, &usage);
#ifdef __APPLE__
    return usage.ru_maxrss;
#else
    return usage.ru_maxrss * 1024;
#endif
}
template<typename T> std::string _to_string(const T& v) {
    std::ostringstream ss;
    ss << std::boolalpha << v;
    return ss.str();
}
template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {
    os << "[";
    for(size_t i=0;i<vec.size();++i){ if(i>0) os << ", "; os << vec[i]; }
    os << "]";
    return os;
}



struct Complex {
    double re;
    double im;
    auto operator<=>(const Complex&) const = default;
};

std::ostream& operator<<(std::ostream& os, const Complex& v) {
    os << '{' << "'re': "<< std::fixed << std::setprecision(1) << v.re
 << ", " << "'im': "<< std::fixed << std::setprecision(1) << v.im
 << '}';
    return os;
}

Complex add(Complex a, Complex b);
Complex mul(Complex a, Complex b);
Complex neg(Complex a);
Complex inv(Complex a);
Complex conj(Complex a);
std::string cstr(Complex a);
int main();

Complex a = Complex{1.0, 1.0};
Complex b = Complex{3.14159, 1.25};

Complex add(Complex a, Complex b) {
    return Complex{(a.re + b.re), (a.im + b.im)};
}

Complex mul(Complex a, Complex b) {
    return Complex{((a.re * b.re) - (a.im * b.im)), ((a.re * b.im) + (a.im * b.re))};
}

Complex neg(Complex a) {
    return Complex{-a.re, -a.im};
}

Complex inv(Complex a) {
    double denom = ((a.re * a.re) + (a.im * a.im));
    return Complex{((double)(a.re) / (denom)), ((double)(-a.im) / (denom))};
}

Complex conj(Complex a) {
    return Complex{a.re, -a.im};
}

std::string cstr(Complex a) {
    std::string s = (std::string("(") + _to_string(a.re));
    if ((a.im >= 0)) {
        s = (((s + std::string("+")) + _to_string(a.im)) + std::string("i)"));
    } else {
        s = ((s + _to_string(a.im)) + std::string("i)"));
    }
    return s;
}

int main() {
    {
        auto __bench_start = _now();
        auto __bench_mem_start = _mem();
        std::cout << (std::string("a:       ") + cstr(a));
        std::cout << std::endl;
        std::cout << (std::string("b:       ") + cstr(b));
        std::cout << std::endl;
        std::cout << (std::string("a + b:   ") + cstr(add(a, b)));
        std::cout << std::endl;
        std::cout << (std::string("a * b:   ") + cstr(mul(a, b)));
        std::cout << std::endl;
        std::cout << (std::string("-a:      ") + cstr(neg(a)));
        std::cout << std::endl;
        std::cout << (std::string("1 / a:   ") + cstr(inv(a)));
        std::cout << std::endl;
        std::cout << (std::string("a̅:       ") + cstr(conj(a)));
        std::cout << std::endl;
        auto __bench_end = _now();
        auto __bench_mem_end = _mem();
        auto __bench_dur = __bench_end - __bench_start;
        auto __bench_mem = __bench_mem_end;
        std::cout << "{\n  \"duration_us\": " << __bench_dur << ",\n  \"memory_bytes\": " << __bench_mem << ",\n  \"name\": \"main\"\n}" << std::endl;
    }
    return 0;
}
