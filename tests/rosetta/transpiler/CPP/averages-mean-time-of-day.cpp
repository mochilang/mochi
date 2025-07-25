// Generated by Mochi transpiler v0.10.40 on 2025-07-26 05:16:31 GMT+7
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <iomanip>
#include <cmath>
#include <optional>
#include <any>
#include <type_traits>
#include <map>


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

double sinApprox(double x);
double cosApprox(double x);
double atanApprox(double x);
double atan2Approx(double y, double x);
int digit(std::string ch);
int parseTwo(std::string s, int idx);
double parseSec(std::string s);
std::string pad(int n);
std::string meanTime(const std::vector<std::string>& times);
int main();

double PI = 3.141592653589793;

double sinApprox(double x) {
    double term = x;
    double sum = x;
    int n = 1;
    while ((n <= 8)) {
        double denom = (double)(((2 * n) * ((2 * n) + 1)));
        term = ((double)(((-term * x) * x)) / (denom));
        sum = (sum + term);
        n = (n + 1);
    }
    return sum;
}

double cosApprox(double x) {
    double term = 1.0;
    double sum = 1.0;
    int n = 1;
    while ((n <= 8)) {
        double denom = (double)((((2 * n) - 1) * (2 * n)));
        term = ((double)(((-term * x) * x)) / (denom));
        sum = (sum + term);
        n = (n + 1);
    }
    return sum;
}

double atanApprox(double x) {
    if ((x > 1.0)) {
        return (((double)(PI) / (2.0)) - ((double)(x) / (((x * x) + 0.28))));
    }
    if ((x < -1.0)) {
        return (((double)(-PI) / (2.0)) - ((double)(x) / (((x * x) + 0.28))));
    }
    return ((double)(x) / ((1.0 + ((0.28 * x) * x))));
}

double atan2Approx(double y, double x) {
    if ((x > 0.0)) {
        return atanApprox(((double)(y) / (x)));
    }
    if ((x < 0.0)) {
        if ((y >= 0.0)) {
            return (atanApprox(((double)(y) / (x))) + PI);
        }
        return (atanApprox(((double)(y) / (x))) - PI);
    }
    if ((y > 0.0)) {
        return ((double)(PI) / (2.0));
    }
    if ((y < 0.0)) {
        return ((double)(-PI) / (2.0));
    }
    return 0.0;
}

int digit(std::string ch) {
    std::string digits = std::string("0123456789");
    int i = 0;
    while ((i < digits.size())) {
        if ((digits.substr(i, (i + 1) - i) == ch)) {
            return i;
        }
        i = (i + 1);
    }
    return 0;
}

int parseTwo(std::string s, int idx) {
    return ((digit(s.substr(idx, (idx + 1) - idx)) * 10) + digit(s.substr((idx + 1), (idx + 2) - (idx + 1))));
}

double parseSec(std::string s) {
    int h = parseTwo(s, 0);
    int m = parseTwo(s, 3);
    int sec = parseTwo(s, 6);
    int tmp = ((((h * 60) + m) * 60) + sec);
    return (double)(tmp);
}

std::string pad(int n) {
    if ((n < 10)) {
        return (std::string("0") + ([&]{ std::ostringstream ss; ss << std::boolalpha << n; return ss.str(); }()));
    }
    return ([&]{ std::ostringstream ss; ss << std::boolalpha << n; return ss.str(); }());
}

std::string meanTime(const std::vector<std::string>& times) {
    double ssum = 0.0;
    double csum = 0.0;
    int i = 0;
    while ((i < times.size())) {
        double sec = parseSec(times[i]);
        double ang = ((double)(((sec * 2.0) * PI)) / (86400.0));
        ssum = (ssum + sinApprox(ang));
        csum = (csum + cosApprox(ang));
        i = (i + 1);
    }
    double theta = atan2Approx(ssum, csum);
    double frac = ((double)(theta) / ((2.0 * PI)));
    while ((frac < 0.0)) {
        frac = (frac + 1.0);
    }
    double total = (frac * 86400.0);
    int si = (int)(total);
    int h = (int)((si / 3600));
    int m = (int)(((si % 3600) / 60));
    int s = (int)((si % 60));
    return ((((pad(h) + std::string(":")) + pad(m)) + std::string(":")) + pad(s));
}

int main() {
    std::vector<std::string> inputs = std::vector<std::string>{std::string("23:00:17"), std::string("23:40:20"), std::string("00:12:45"), std::string("00:17:19")};
    std::cout << meanTime(inputs);
    std::cout << std::endl;
    return 0;
}
