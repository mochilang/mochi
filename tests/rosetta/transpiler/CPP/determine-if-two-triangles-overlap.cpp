// Generated by Mochi transpiler v0.10.47 on 2025-07-28 11:47:48 GMT+7
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


static long _index_of(const std::string& s, const std::string& sub) {
    auto pos = s.find(sub);
    return pos == std::string::npos ? -1 : static_cast<long>(pos);
}
template<typename T> long _index_of(const std::vector<T>& xs, const T& v) {
    for(size_t i=0;i<xs.size();++i){ if(xs[i]==v) return i; }
    return -1;
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


struct Point {
    double x;
    double y;
    auto operator<=>(const Point&) const = default;
};

struct Triangle {
    Point p1;
    Point p2;
    Point p3;
    auto operator<=>(const Triangle&) const = default;
};

std::ostream& operator<<(std::ostream& os, const Point& v) {
    os << '{' << "'x': "<< std::fixed << std::setprecision(1) << v.x
 << ", " << "'y': "<< std::fixed << std::setprecision(1) << v.y
 << '}';
    return os;
}

std::ostream& operator<<(std::ostream& os, const Triangle& v) {
    os << '{' << "'p1': "<< v.p1
 << ", " << "'p2': "<< v.p2
 << ", " << "'p3': "<< v.p3
 << '}';
    return os;
}

std::string fmt1(double f);
std::string pointStr(const Point& p);
std::string triangleStr(const Triangle& t);
double orient(const Point& a, const Point& b, const Point& c);
bool pointInTri(const Point& p, const Triangle& t, bool onBoundary);
bool edgeCheck(const Point& a0, const Point& a1, const std::vector<Point>& bs, bool onBoundary);
bool triTri2D(const Triangle& t1, const Triangle& t2, bool onBoundary);
std::string iff(bool cond, std::string a, std::string b);
int main();

std::string fmt1(double f) {
    std::string s = ([&]{ std::ostringstream ss; ss << std::boolalpha << f; return ss.str(); }());
    int64_t idx = _index_of(s, std::string("."));
    if ((idx < int64_t(0))) {
        s = (s + std::string(".0"));
    } else {
        int64_t need = (idx + int64_t(2));
        if ((s.size() > need)) {
            s = s.substr(int64_t(0), need - int64_t(0));
        } else {
            while ((s.size() < need)) {
                s = (s + std::string("0"));
            }
        }
    }
    return s;
}

std::string pointStr(const Point& p) {
    return ((((std::string("(") + fmt1(p.x)) + std::string(", ")) + fmt1(p.y)) + std::string(")"));
}

std::string triangleStr(const Triangle& t) {
    return (((((std::string("Triangle ") + pointStr(t.p1)) + std::string(", ")) + pointStr(t.p2)) + std::string(", ")) + pointStr(t.p3));
}

double orient(const Point& a, const Point& b, const Point& c) {
    return (((b.x - a.x) * (c.y - a.y)) - ((b.y - a.y) * (c.x - a.x)));
}

bool pointInTri(const Point& p, const Triangle& t, bool onBoundary) {
    double d1 = orient(p, t.p1, t.p2);
    double d2 = orient(p, t.p2, t.p3);
    double d3 = orient(p, t.p3, t.p1);
    bool hasNeg = (((d1 < 0.0) || (d2 < 0.0)) || (d3 < 0.0));
    bool hasPos = (((d1 > 0.0) || (d2 > 0.0)) || (d3 > 0.0));
    if (onBoundary) {
        return !(hasNeg && hasPos);
    }
    return (((!(hasNeg && hasPos) && (d1 != 0.0)) && (d2 != 0.0)) && (d3 != 0.0));
}

bool edgeCheck(const Point& a0, const Point& a1, const std::vector<Point>& bs, bool onBoundary) {
    double d0 = orient(a0, a1, bs[int64_t(0)]);
    double d1 = orient(a0, a1, bs[int64_t(1)]);
    double d2 = orient(a0, a1, bs[int64_t(2)]);
    if (onBoundary) {
        return (((d0 <= 0.0) && (d1 <= 0.0)) && (d2 <= 0.0));
    }
    return (((d0 < 0.0) && (d1 < 0.0)) && (d2 < 0.0));
}

bool triTri2D(const Triangle& t1, const Triangle& t2, bool onBoundary) {
    std::vector<Point> a = std::vector<Point>{t1.p1, t1.p2, t1.p3};
    std::vector<Point> b = std::vector<Point>{t2.p1, t2.p2, t2.p3};
    int64_t i = int64_t(0);
    while ((i < int64_t(3))) {
        int64_t j = ((i + int64_t(1)) % int64_t(3));
        if (edgeCheck(a[i], a[j], b, onBoundary)) {
            return false;
        }
        i = (i + int64_t(1));
    }
    i = int64_t(0);
    while ((i < int64_t(3))) {
        int64_t j = ((i + int64_t(1)) % int64_t(3));
        if (edgeCheck(b[i], b[j], a, onBoundary)) {
            return false;
        }
        i = (i + int64_t(1));
    }
    return true;
}

std::string iff(bool cond, std::string a, std::string b) {
    if (cond) {
        return a;
    } else {
        return b;
    }
}

int main() {
    Triangle t1 = Triangle{.p1 = Point{.x = 0.0, .y = 0.0}, .p2 = Point{.x = 5.0, .y = 0.0}, .p3 = Point{.x = 0.0, .y = 5.0}};
    Triangle t2 = Triangle{.p1 = Point{.x = 0.0, .y = 0.0}, .p2 = Point{.x = 5.0, .y = 0.0}, .p3 = Point{.x = 0.0, .y = 6.0}};
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    bool overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    t1 = Triangle{.p1 = Point{.x = 0.0, .y = 0.0}, .p2 = Point{.x = 0.0, .y = 5.0}, .p3 = Point{.x = 5.0, .y = 0.0}};
    t2 = t1;
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap (reversed)"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    t1 = Triangle{.p1 = Point{.x = 0.0, .y = 0.0}, .p2 = Point{.x = 5.0, .y = 0.0}, .p3 = Point{.x = 0.0, .y = 5.0}};
    t2 = Triangle{.p1 = Point{.x = -10.0, .y = 0.0}, .p2 = Point{.x = -5.0, .y = 0.0}, .p3 = Point{.x = -1.0, .y = 6.0}};
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    t1.p3 = Point{.x = 2.5, .y = 5.0};
    t2 = Triangle{.p1 = Point{.x = 0.0, .y = 4.0}, .p2 = Point{.x = 2.5, .y = -1.0}, .p3 = Point{.x = 5.0, .y = 4.0}};
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    t1 = Triangle{.p1 = Point{.x = 0.0, .y = 0.0}, .p2 = Point{.x = 1.0, .y = 1.0}, .p3 = Point{.x = 0.0, .y = 2.0}};
    t2 = Triangle{.p1 = Point{.x = 2.0, .y = 1.0}, .p2 = Point{.x = 3.0, .y = 0.0}, .p3 = Point{.x = 3.0, .y = 2.0}};
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    t2 = Triangle{.p1 = Point{.x = 2.0, .y = 1.0}, .p2 = Point{.x = 3.0, .y = -2.0}, .p3 = Point{.x = 3.0, .y = 4.0}};
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    t1 = Triangle{.p1 = Point{.x = 0.0, .y = 0.0}, .p2 = Point{.x = 1.0, .y = 0.0}, .p3 = Point{.x = 0.0, .y = 1.0}};
    t2 = Triangle{.p1 = Point{.x = 1.0, .y = 0.0}, .p2 = Point{.x = 2.0, .y = 0.0}, .p3 = Point{.x = 1.0, .y = 1.1}};
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    std::cout << std::string("which have only a single corner in contact, if boundary points collide");
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, true);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    std::cout << std::string("");
    std::cout << std::endl;
    std::cout << (triangleStr(t1) + std::string(" and"));
    std::cout << std::endl;
    std::cout << triangleStr(t2);
    std::cout << std::endl;
    std::cout << std::string("which have only a single corner in contact, if boundary points do not collide");
    std::cout << std::endl;
    overlapping = triTri2D(t1, t2, false);
    std::cout << iff(overlapping, std::string("overlap"), std::string("do not overlap"));
    std::cout << std::endl;
    return 0;
}
