// Generated by Mochi transpiler v0.10.41 on 2025-07-27 11:45:13 GMT+7
#include <iostream>
#include <string>
#include <any>
#include <map>
#include <vector>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <type_traits>
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
static long long _bench_now() {
    return std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::system_clock::now().time_since_epoch()).count();
}
static void any_to_stream(std::ostream& os, const std::any& val) {
    if(val.type() == typeid(int)) os << std::any_cast<int>(val);
    else if(val.type() == typeid(int64_t)) os << std::any_cast<int64_t>(val);
    else if(val.type() == typeid(double)) os << std::any_cast<double>(val);
    else if(val.type() == typeid(bool)) os << (std::any_cast<bool>(val) ? "true" : "false");
    else if(val.type() == typeid(std::string)) os << std::any_cast<std::string>(val);
    else if(val.type() == typeid(std::vector<int64_t>)) { const auto& v = std::any_cast<const std::vector<int64_t>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; os << v[i]; } os << ']'; }
    else if(val.type() == typeid(std::vector<std::vector<int64_t>>)) { const auto& vv = std::any_cast<const std::vector<std::vector<int64_t>>&>(val); os << '['; for(size_t i=0;i<vv.size();++i){ if(i>0) os << ', '; const auto& v = vv[i]; os << '['; for(size_t j=0;j<v.size();++j){ if(j>0) os << ', '; os << v[j]; } os << ']'; } os << ']'; }
    else if(val.type() == typeid(std::vector<std::string>)) { const auto& v = std::any_cast<const std::vector<std::string>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; os << v[i]; } os << ']'; }
    else if(val.type() == typeid(std::vector<std::any>)) { const auto& v = std::any_cast<const std::vector<std::any>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; any_to_stream(os, v[i]); } os << ']'; }
    else if(val.type() == typeid(std::map<std::string, std::any>)) { const auto& m = std::any_cast<const std::map<std::string, std::any>&>(val); os << '{'; bool first=true; for(const auto& p : m){ if(!first) os << ', '; first=false; os << p.first << ': '; any_to_stream(os, p.second); } os << '}'; }
    else os << "<any>";
}
static double any_to_double(const std::any& v) {
    if(v.type() == typeid(int)) return (double)std::any_cast<int>(v);
    if(v.type() == typeid(double)) return std::any_cast<double>(v);
    return 0;
}
template <typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& vec) {
    os << "[";
    for(size_t i=0;i<vec.size();++i){ if(i>0) os << ", "; if constexpr(std::is_same_v<T, std::any>) any_to_stream(os, vec[i]); else os << vec[i]; }
    os << "]";
    return os;
}


template<typename T> std::string _to_string(const T& v) {
    std::ostringstream ss;
    ss << std::boolalpha << v;
    return ss.str();
}

bool contains(std::string s, std::string ch);
std::vector<std::string> sortStrings(const std::vector<std::string>& xs);
std::map<std::string, std::any> bwt(std::string s);
std::string ibwt(std::string r);
std::string makePrintable(std::string s);
int main();

std::string stx = std::string("\x02");
std::string etx = std::string("\x03");

bool contains(std::string s, std::string ch) {
    int64_t i = 0;
    while ((i < s.size())) {
        if ((s.substr(i, (i + 1) - i) == ch)) {
            return true;
        }
        i = (i + 1);
    }
    return false;
}

std::vector<std::string> sortStrings(const std::vector<std::string>& xs) {
    std::vector<std::string> arr = xs;
    int n = arr.size();
    int64_t i = 0;
    while ((i < n)) {
        int64_t j = 0;
        while ((j < (n - 1))) {
            if ((arr[j] > arr[(j + 1)])) {
                std::string tmp = arr[j];
                arr[j] = arr[(j + 1)];
                arr[(j + 1)] = tmp;
            }
            j = (j + 1);
        }
        i = (i + 1);
    }
    return arr;
}

std::map<std::string, std::any> bwt(std::string s) {
    if (((double)((s.find(stx) != std::string::npos)) || (double)((s.find(etx) != std::string::npos)))) {
        return std::map<std::string, std::any>{{"err", std::any(true)}, {"res", std::any(std::string(""))}};
    }
    s = ((stx + s) + etx);
    int le = s.size();
    std::vector<std::string> table = {};
    int64_t i = 0;
    while ((i < le)) {
        std::string rot = (s.substr(i, le - i) + s.substr(0, i - 0));
        table = ([&]{ auto __tmp = table; __tmp.push_back(rot); return __tmp; }());
        i = (i + 1);
    }
    table = sortStrings(table);
    std::string last = std::string("");
    i = 0;
    while ((i < le)) {
        last = (last + table[i].substr((le - 1), le - (le - 1)));
        i = (i + 1);
    }
    return std::map<std::string, std::any>{{"err", std::any(false)}, {"res", std::any(last)}};
}

std::string ibwt(std::string r) {
    int le = r.size();
    std::vector<std::string> table = {};
    int64_t i = 0;
    while ((i < le)) {
        table = ([&]{ auto __tmp = table; __tmp.push_back(std::string("")); return __tmp; }());
        i = (i + 1);
    }
    int64_t n = 0;
    while ((n < le)) {
        i = 0;
        while ((i < le)) {
            table[i] = (r.substr(i, (i + 1) - i) + table[i]);
            i = (i + 1);
        }
        table = sortStrings(table);
        n = (n + 1);
    }
    i = 0;
    while ((i < le)) {
        if ((table[i].substr((le - 1), le - (le - 1)) == etx)) {
            return table[i].substr(1, (le - 1) - 1);
        }
        i = (i + 1);
    }
    return std::string("");
}

std::string makePrintable(std::string s) {
    std::string out = std::string("");
    int64_t i = 0;
    while ((i < s.size())) {
        std::string ch = s.substr(i, (i + 1) - i);
        if ((ch == stx)) {
            out = (out + std::string("^"));
        } else         if ((ch == etx)) {
            out = (out + std::string("|"));
        } else {
            out = (out + ch);
        }

        i = (i + 1);
    }
    return out;
}

int main() {
    {
        struct __BenchGuard {
            long long start;
            long memStart;
            __BenchGuard() : start(_bench_now()), memStart(_mem()) {}
            ~__BenchGuard() {
                    auto __bench_end = _bench_now();
                    auto __bench_mem_end = _mem();
                    auto __bench_dur = __bench_end - start;
                    auto __bench_mem = __bench_mem_end;
                    std::cout << "{\n  \"duration_us\": " << __bench_dur << ",\n  \"memory_bytes\": " << __bench_mem << ",\n  \"name\": \"main\"\n}" << std::endl;
            }
        } __bench_guard;
        std::vector<std::string> examples = std::vector<std::string>{std::string("banana"), std::string("appellee"), std::string("dogwood"), std::string("TO BE OR NOT TO BE OR WANT TO BE OR NOT?"), std::string("SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES"), std::string("\x02ABC\x03")};
        for (std::string t : examples) {
            std::cout << makePrintable(t);
            std::cout << std::endl;
            std::map<std::string, std::any> res = bwt(t);
            if (std::any_cast<bool>(res[std::string("err")])) {
                std::cout << std::string(" --> ERROR: String can't contain STX or ETX");
                std::cout << std::endl;
                std::cout << std::string(" -->");
                std::cout << std::endl;
            } else {
                std::string enc = std::any_cast<std::string>(res[std::string("res")]);
                std::cout << (std::string(" --> ") + makePrintable(enc));
                std::cout << std::endl;
                std::string r = ibwt(enc);
                std::cout << (std::string(" --> ") + r);
                std::cout << std::endl;
            }
            std::cout << std::string("");
            std::cout << std::endl;
        }
    }
    return 0;
}
