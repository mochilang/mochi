// Generated by Mochi transpiler v0.10.54 on 2025-08-02 12:08:28 GMT+7
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
#include <cstdlib>
#include <chrono>


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

std::any gzipWriter(std::any w);
std::any tarWriter(std::any w);
void tarWriteHeader(std::any w, const std::map<std::string, std::any>& hdr);
void tarWrite(std::any w, std::string data);
int main();

std::any gzipWriter(std::any w) {
    return w;
}

std::any tarWriter(std::any w) {
    return w;
}

void tarWriteHeader(std::any w, const std::map<std::string, std::any>& hdr) {
}

void tarWrite(std::any w, std::string data) {
}

int main() {
    std::string filename = std::string("TAPE.FILE");
    std::string data = std::string("");
    std::string outfile = std::string("");
    bool gzipFlag = false;
    std::any w = nullptr;
    if ((outfile != std::string(""))) {
        w = nullptr;
    }
    if (gzipFlag) {
        w = gzipWriter(w);
    }
    w = tarWriter(w);
    std::map<std::string, std::any> hdr = std::map<std::string, std::any>{{"Name", std::any(filename)}, {"Mode", std::any(int64_t(432))}, {"Size", std::any(data.size())}, {"ModTime", std::any(_now())}, {"Typeflag", std::any(int64_t(0))}, {"Uname", std::any(std::string("guest"))}, {"Gname", std::any(std::string("guest"))}};
    tarWriteHeader(w, hdr);
    tarWrite(w, data);
    return 0;
}
