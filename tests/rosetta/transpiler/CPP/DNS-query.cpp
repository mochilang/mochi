// Generated by Mochi transpiler v0.10.40 on 2025-07-25 18:43:42 GMT+7
#include <iostream>
#include <string>
#include <any>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <vector>
#include <cstdlib>
#include <chrono>
#include <sys/resource.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <cstring>
#include <map>


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
static std::vector<std::any> _lookup_host(const std::string& host) {
    std::vector<std::any> res;
    std::vector<std::string> ips;
    addrinfo hints{}; memset(&hints, 0, sizeof(hints)); hints.ai_family = AF_INET;
    addrinfo* info=nullptr;
    int rc = getaddrinfo(host.c_str(), nullptr, &hints, &info);
    if(rc != 0) {
        res.push_back(ips);
        res.push_back(std::string(gai_strerror(rc)));
        return res;
    }
    for(auto p=info; p; p=p->ai_next){
        char buf[INET_ADDRSTRLEN];
        auto* addr = reinterpret_cast<sockaddr_in*>(p->ai_addr);
        if(inet_ntop(AF_INET, &addr->sin_addr, buf, sizeof(buf))) ips.push_back(std::string(buf));
    }
    freeaddrinfo(info);
    res.push_back(ips);
    res.push_back(std::any());
    return res;
}
static void any_to_stream(std::ostream& os, const std::any& val) {
    if(val.type() == typeid(int)) os << std::any_cast<int>(val);
    else if(val.type() == typeid(double)) os << std::any_cast<double>(val);
    else if(val.type() == typeid(bool)) os << (std::any_cast<bool>(val) ? "true" : "false");
    else if(val.type() == typeid(std::string)) os << std::any_cast<std::string>(val);
    else if(val.type() == typeid(std::vector<int>)) { const auto& v = std::any_cast<const std::vector<int>&>(val); os << '['; for(size_t i=0;i<v.size();++i){ if(i>0) os << ', '; os << v[i]; } os << ']'; }
    else if(val.type() == typeid(std::vector<std::vector<int>>)) { const auto& vv = std::any_cast<const std::vector<std::vector<int>>&>(val); os << '['; for(size_t i=0;i<vv.size();++i){ if(i>0) os << ', '; const auto& v = vv[i]; os << '['; for(size_t j=0;j<v.size();++j){ if(j>0) os << ', '; os << v[j]; } os << ']'; } os << ']'; }
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

int main();

std::vector<std::any> res = _lookup_host(std::string("www.kame.net"));
std::any addrs = res[0];
std::any err = res[1];

int main() {
    {
        auto __bench_start = _now();
        auto __bench_mem_start = _mem();
        if ((!err.has_value())) {
            std::cout << ([&]{ std::ostringstream ss; any_to_stream(ss, addrs); return ss.str(); }());
            std::cout << std::endl;
        } else {
            any_to_stream(std::cout, err);
            std::cout << std::endl;
        }
        auto __bench_end = _now();
        auto __bench_mem_end = _mem();
        auto __bench_dur = __bench_end - __bench_start;
        auto __bench_mem = __bench_mem_end;
        std::cout << "{\n  \"duration_us\": " << __bench_dur << ",\n  \"memory_bytes\": " << __bench_mem << ",\n  \"name\": \"main\"\n}" << std::endl;
    }
    return 0;
}
