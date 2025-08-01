// Generated by Mochi transpiler v0.10.47 on 2025-07-28 11:47:36 GMT+7
#include <iostream>
#include <string>
#include <vector>
#include <functional>
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

double sqrtApprox(double x);
std::vector<double> funnel(const std::vector<double>& fa, std::function<double(double, double)> r);
double mean(const std::vector<double>& fa);
double stdDev(const std::vector<double>& fa);
void experiment(std::string label, std::function<double(double, double)> r);
int main();

std::vector<double> dxs = std::vector<double>{-0.533, 0.27, 0.859, -0.043, -0.205, -0.127, -0.071, 0.275, 1.251, -0.231, -0.401, 0.269, 0.491, 0.951, 1.15, 0.001, -0.382, 0.161, 0.915, 2.08, -2.337, 0.034, -0.126, 0.014, 0.709, 0.129, -1.093, -0.483, -1.193, 0.02, -0.051, 0.047, -0.095, 0.695, 0.34, -0.182, 0.287, 0.213, -0.423, -0.021, -0.134, 1.798, 0.021, -1.099, -0.361, 1.636, -1.134, 1.315, 0.201, 0.034, 0.097, -0.17, 0.054, -0.553, -0.024, -0.181, -0.7, -0.361, -0.789, 0.279, -0.174, -0.009, -0.323, -0.658, 0.348, -0.528, 0.881, 0.021, -0.853, 0.157, 0.648, 1.774, -1.043, 0.051, 0.021, 0.247, -0.31, 0.171, 0.0, 0.106, 0.024, -0.386, 0.962, 0.765, -0.125, -0.289, 0.521, 0.017, 0.281, -0.749, -0.149, -2.436, -0.909, 0.394, -0.113, -0.598, 0.443, -0.521, -0.799, 0.087};
std::vector<double> dys = std::vector<double>{0.136, 0.717, 0.459, -0.225, 1.392, 0.385, 0.121, -0.395, 0.49, -0.682, -0.065, 0.242, -0.288, 0.658, 0.459, 0.0, 0.426, 0.205, -0.765, -2.188, -0.742, -0.01, 0.089, 0.208, 0.585, 0.633, -0.444, -0.351, -1.087, 0.199, 0.701, 0.096, -0.025, -0.868, 1.051, 0.157, 0.216, 0.162, 0.249, -0.007, 0.009, 0.508, -0.79, 0.723, 0.881, -0.508, 0.393, -0.226, 0.71, 0.038, -0.217, 0.831, 0.48, 0.407, 0.447, -0.295, 1.126, 0.38, 0.549, -0.445, -0.046, 0.428, -0.074, 0.217, -0.822, 0.491, 1.347, -0.141, 1.23, -0.044, 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, -0.729, 0.65, -1.103, 0.154, -1.72, 0.051, -0.385, 0.477, 1.537, -0.901, 0.939, -0.411, 0.341, -0.411, 0.106, 0.224, -0.947, -1.424, -0.542, -1.032};

double sqrtApprox(double x) {
    if ((x <= 0.0)) {
        return 0.0;
    }
    double g = x;
    int64_t i = int64_t(0);
    while ((i < int64_t(20))) {
        g = ((double)((g + ((double)(x) / (g)))) / (2.0));
        i = (i + int64_t(1));
    }
    return g;
}

std::vector<double> funnel(const std::vector<double>& fa, std::function<double(double, double)> r) {
    double x = 0.0;
    std::vector<int64_t> result = {};
    int64_t i = int64_t(0);
    while ((i < fa.size())) {
        double f = fa[i];
        result = ([&]{ auto __tmp = result; __tmp.push_back((int64_t)(x + f)); return __tmp; }());
        x = r(x, f);
        i = (i + int64_t(1));
    }
    return result;
}

double mean(const std::vector<double>& fa) {
    double sum = 0.0;
    int64_t i = int64_t(0);
    while ((i < fa.size())) {
        sum = (sum + fa[i]);
        i = (i + int64_t(1));
    }
    return ((double)(sum) / ((double)(fa.size())));
}

double stdDev(const std::vector<double>& fa) {
    double m = mean(fa);
    double sum = 0.0;
    int64_t i = int64_t(0);
    while ((i < fa.size())) {
        double d = (fa[i] - m);
        sum = (sum + (d * d));
        i = (i + int64_t(1));
    }
    double r = sqrtApprox(((double)(sum) / ((double)(fa.size()))));
    return r;
}

void experiment(std::string label, std::function<double(double, double)> r) {
    std::vector<double> rxs = funnel(dxs, r);
    std::vector<double> rys = funnel(dys, r);
    std::cout << (label + std::string("  :      x        y"));
    std::cout << std::endl;
    std::cout << (((std::string("Mean    :  ") + ([&]{ std::ostringstream ss; ss << std::boolalpha << mean(rxs); return ss.str(); }())) + std::string(", ")) + ([&]{ std::ostringstream ss; ss << std::boolalpha << mean(rys); return ss.str(); }()));
    std::cout << std::endl;
    std::cout << (((std::string("Std Dev :  ") + ([&]{ std::ostringstream ss; ss << std::boolalpha << stdDev(rxs); return ss.str(); }())) + std::string(", ")) + ([&]{ std::ostringstream ss; ss << std::boolalpha << stdDev(rys); return ss.str(); }()));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
}

int main() {
    experiment(std::string("Rule 1"), (std::function<double(double, double)>)([&](double x, double y) { return 0.0; }));
    experiment(std::string("Rule 2"), (std::function<double(double, double)>)([&](double x, double dz) { return -dz; }));
    experiment(std::string("Rule 3"), (std::function<double(double, double)>)([&](double z, double dz) { return -((double)(z) + (double)(dz)); }));
    experiment(std::string("Rule 4"), (std::function<double(double, double)>)([&](double z, double dz) { return ((double)(z) + (double)(dz)); }));
    return 0;
}
