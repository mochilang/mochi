// Generated by Mochi transpiler v0.10.52 on 2025-08-01 21:18:20 GMT+7
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <any>
#include <type_traits>
#include <map>


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
static std::string any_to_string(const std::any& v) {
    if(v.type() == typeid(std::string)) return std::any_cast<std::string>(v);
    if(v.type() == typeid(int)) return std::to_string(std::any_cast<int>(v));
    if(v.type() == typeid(int64_t)) return std::to_string(std::any_cast<int64_t>(v));
    if(v.type() == typeid(double)) { std::ostringstream ss; ss << std::any_cast<double>(v); return ss.str(); }
    if(v.type() == typeid(bool)) return std::any_cast<bool>(v) ? "true" : "false";
    return std::string();
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

std::vector<int64_t> primesUpTo(int64_t n);
void longestSeq(std::string dir);
int main();

int64_t LIMIT = int64_t(999999);
std::vector<int64_t> primes = primesUpTo(LIMIT);

std::vector<int64_t> primesUpTo(int64_t n) {
    std::vector<bool> sieve = {};
    int64_t i = int64_t(0);
    while ((i <= n)) {
        sieve = ([&]{ auto __tmp = sieve; __tmp.push_back(true); return __tmp; }());
        i = (i + int64_t(1));
    }
    int64_t p = int64_t(2);
    while (((p * p) <= n)) {
        if (sieve[p]) {
            int64_t m = (p * p);
            while ((m <= n)) {
                sieve[m] = false;
                m = (m + p);
            }
        }
        p = (p + int64_t(1));
    }
    std::vector<int64_t> res = {};
    int64_t x = int64_t(2);
    while ((x <= n)) {
        if (sieve[x]) {
            res = ([&]{ auto __tmp = res; __tmp.push_back(x); return __tmp; }());
        }
        x = (x + int64_t(1));
    }
    return res;
}

void longestSeq(std::string dir) {
    int64_t pd = int64_t(0);
    std::vector<std::vector<int64_t>> longSeqs = std::vector<std::vector<int64_t>>{std::vector<int64_t>{int64_t(2)}};
    std::vector<int64_t> currSeq = std::vector<int64_t>{int64_t(2)};
    int64_t i = int64_t(1);
    while ((i < primes.size())) {
        int64_t d = (primes[i] - primes[(i - int64_t(1))]);
        if ((((dir == std::string("ascending")) && (d <= pd)) || ((dir == std::string("descending")) && (d >= pd)))) {
            if ((currSeq.size() > longSeqs[int64_t(0)].size())) {
                longSeqs = std::vector<std::vector<int64_t>>{currSeq};
            } else             if ((currSeq.size() == longSeqs[int64_t(0)].size())) {
                longSeqs = ([&]{ auto __tmp = longSeqs; __tmp.push_back(currSeq); return __tmp; }());
            }

            currSeq = std::vector<int64_t>{primes[(i - int64_t(1))], primes[i]};
        } else {
            currSeq = ([&]{ auto __tmp = currSeq; __tmp.push_back(primes[i]); return __tmp; }());
        }
        pd = d;
        i = (i + int64_t(1));
    }
    if ((currSeq.size() > longSeqs[int64_t(0)].size())) {
        longSeqs = std::vector<std::vector<int64_t>>{currSeq};
    } else     if ((currSeq.size() == longSeqs[int64_t(0)].size())) {
        longSeqs = ([&]{ auto __tmp = longSeqs; __tmp.push_back(currSeq); return __tmp; }());
    }

    std::cout << ((((std::string("Longest run(s) of primes with ") + dir) + std::string(" differences is ")) + ([&]{ std::ostringstream ss; ss << std::boolalpha << longSeqs[int64_t(0)].size(); return ss.str(); }())) + std::string(" :"));
    std::cout << std::endl;
    for (std::vector<int64_t> ls : longSeqs) {
        std::vector<int64_t> diffs = {};
        int64_t j = int64_t(1);
        while ((j < ls.size())) {
            diffs = ([&]{ auto __tmp = diffs; __tmp.push_back((ls[j] - ls[(j - int64_t(1))])); return __tmp; }());
            j = (j + int64_t(1));
        }
        int64_t k = int64_t(0);
        while ((k < (ls.size() - int64_t(1)))) {
            std::cout << (((([&]{ std::ostringstream ss; ss << std::boolalpha << ls[k]; return ss.str(); }()) + std::string(" (")) + ([&]{ std::ostringstream ss; ss << std::boolalpha << diffs[k]; return ss.str(); }())) + std::string(") "));
            std::cout << " ";
            std::cout << (false ? "true" : "false");
            std::cout << std::endl;
            k = (k + int64_t(1));
        }
        std::cout << ([&]{ std::ostringstream ss; ss << std::boolalpha << ls[(ls.size() - int64_t(1))]; return ss.str(); }());
        std::cout << std::endl;
    }
    std::cout << std::string("");
    std::cout << std::endl;
}

int main() {
    std::cout << std::string("For primes < 1 million:\n");
    std::cout << std::endl;
    for (std::string dir : std::vector<std::string>{std::string("ascending"), std::string("descending")}) {
        longestSeq(dir);
    }
    return 0;
}
