// Generated by Mochi transpiler v0.10.41 on 2025-07-27 16:07:14 GMT+7
#include <iostream>
#include <string>
#include <any>
#include <map>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <vector>
#include <type_traits>


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

void gifEncode(std::any out, std::any img, const std::map<std::string, int64_t>& opts);
int main();

void gifEncode(std::any out, std::any img, const std::map<std::string, int64_t>& opts) {
}

int main() {
    std::map<std::string, int64_t> opts = {};
    opts[std::string("NumColors")] = int64_t(16);
    gifEncode(nullptr, nullptr, opts);
    return 0;
}
