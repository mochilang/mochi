// Generated by Mochi transpiler v0.10.40 on 2025-07-26 10:00:44 GMT+7
#include <iostream>
#include <string>
#include <vector>
#include <type_traits>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <any>
#include <openssl/sha.h>
#include <map>


static std::vector<int64_t> _sha256(const std::vector<int64_t>& bs) {
    unsigned char digest[SHA256_DIGEST_LENGTH];
    std::vector<unsigned char> data(bs.size());
    for(size_t i=0;i<bs.size();++i) data[i]=static_cast<unsigned char>(bs[i]);
    SHA256(data.data(), data.size(), digest);
    std::vector<int64_t> out(SHA256_DIGEST_LENGTH);
    for(int i=0;i<SHA256_DIGEST_LENGTH;i++) out[i]=digest[i];
    return out;
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

int64_t indexOf(std::string s, std::string ch);
std::vector<int64_t> set58(std::string addr);
std::vector<int64_t> doubleSHA256(const std::vector<int64_t>& bs);
std::vector<int64_t> computeChecksum(const std::vector<int64_t>& a);
bool validA58(std::string addr);
int main();

int64_t indexOf(std::string s, std::string ch) {
    int64_t i = 0;
    while ((i < s.size())) {
        if ((([&](const auto& __v){ if constexpr(std::is_same_v<std::decay_t<decltype(__v)>, std::string>) return __v.substr(i, (i + 1) - i); else return std::vector<typename std::decay_t<decltype(__v)>::value_type>(__v.begin()+i, __v.begin()+(i + 1)); })(s) == ch)) {
            return i;
        }
        i = (i + 1);
    }
    return -1;
}

std::vector<int64_t> set58(std::string addr) {
    std::string tmpl = std::string("123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz");
    std::vector<int64_t> a = {};
    int64_t i = 0;
    while ((i < 25)) {
        a = ([&]{ auto __tmp = a; __tmp.push_back(0); return __tmp; }());
        i = (i + 1);
    }
    int64_t idx = 0;
    while ((idx < addr.size())) {
        std::string ch = ([&](const auto& __v){ if constexpr(std::is_same_v<std::decay_t<decltype(__v)>, std::string>) return __v.substr(idx, (idx + 1) - idx); else return std::vector<typename std::decay_t<decltype(__v)>::value_type>(__v.begin()+idx, __v.begin()+(idx + 1)); })(addr);
        int64_t c = indexOf(tmpl, ch);
        if ((c < 0)) {
            return {};
        }
        int64_t j = 24;
        while ((j >= 0)) {
            c = (c + (58 * a[j]));
            a[j] = (c % 256);
            c = (int64_t)((c / 256));
            j = (j - 1);
        }
        if ((c > 0)) {
            return {};
        }
        idx = (idx + 1);
    }
    return a;
}

std::vector<int64_t> doubleSHA256(const std::vector<int64_t>& bs) {
    std::vector<int64_t> first = _sha256(bs);
    return _sha256(first);
}

std::vector<int64_t> computeChecksum(const std::vector<int64_t>& a) {
    std::vector<int64_t> hash = doubleSHA256(([&](const auto& __v){ if constexpr(std::is_same_v<std::decay_t<decltype(__v)>, std::string>) return __v.substr(0, 21 - 0); else return std::vector<typename std::decay_t<decltype(__v)>::value_type>(__v.begin()+0, __v.begin()+21); })(a));
    return ([&](const auto& __v){ if constexpr(std::is_same_v<std::decay_t<decltype(__v)>, std::string>) return __v.substr(0, 4 - 0); else return std::vector<typename std::decay_t<decltype(__v)>::value_type>(__v.begin()+0, __v.begin()+4); })(hash);
}

bool validA58(std::string addr) {
    std::vector<int64_t> a = set58(addr);
    if ((a.size() != 25)) {
        return false;
    }
    if ((a[0] != 0)) {
        return false;
    }
    std::vector<int64_t> sum = computeChecksum(a);
    int64_t i = 0;
    while ((i < 4)) {
        if ((a[(21 + i)] != sum[i])) {
            return false;
        }
        i = (i + 1);
    }
    return true;
}

int main() {
    std::cout << ([&]{ std::ostringstream ss; ss << std::boolalpha << validA58(std::string("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")); return ss.str(); }());
    std::cout << std::endl;
    std::cout << ([&]{ std::ostringstream ss; ss << std::boolalpha << validA58(std::string("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j")); return ss.str(); }());
    std::cout << std::endl;
    return 0;
}
