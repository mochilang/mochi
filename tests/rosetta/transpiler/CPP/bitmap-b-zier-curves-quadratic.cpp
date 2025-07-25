// Generated by Mochi transpiler v0.10.40 on 2025-07-26 10:14:08 GMT+7
#include <iostream>
#include <string>
#include <any>
#include <vector>
#include <map>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
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


struct Pixel {
    int64_t r;
    int64_t g;
    int64_t b;
    auto operator<=>(const Pixel&) const = default;
};

std::ostream& operator<<(std::ostream& os, const Pixel& v) {
    os << '{' << "'r': "<< v.r
 << ", " << "'g': "<< v.g
 << ", " << "'b': "<< v.b
 << '}';
    return os;
}

Pixel pixelFromRgb(int64_t rgb);
std::map<std::string, std::any> newBitmap(int64_t cols, int64_t rows);
void setPx(std::map<std::string, std::any> b, int64_t x, int64_t y, Pixel p);
void fill(std::map<std::string, std::any> b, Pixel p);
void fillRgb(const std::map<std::string, std::any>& b, int64_t rgb);
void line(const std::map<std::string, std::any>& b, int64_t x0, int64_t y0, int64_t x1, int64_t y1, Pixel p);
void bezier2(const std::map<std::string, std::any>& b, int64_t x1, int64_t y1, int64_t x2, int64_t y2, int64_t x3, int64_t y3, Pixel p);
int main();

int64_t b2Seg = 20;
std::map<std::string, std::any> b = newBitmap(400, 300);

Pixel pixelFromRgb(int64_t rgb) {
    int64_t r = (int64_t)(((rgb / 65536) % 256));
    int64_t g = (int64_t)(((rgb / 256) % 256));
    int64_t b = (int64_t)((rgb % 256));
    return Pixel{r, g, b};
}

std::map<std::string, std::any> newBitmap(int64_t cols, int64_t rows) {
    std::vector<std::vector<Pixel>> d = {};
    int64_t y = 0;
    while ((y < rows)) {
        std::vector<Pixel> row = {};
        int64_t x = 0;
        while ((x < cols)) {
            row = ([&]{ auto __tmp = row; __tmp.push_back(Pixel{0, 0, 0}); return __tmp; }());
            x = (x + 1);
        }
        d = ([&]{ auto __tmp = d; __tmp.push_back(row); return __tmp; }());
        y = (y + 1);
    }
    return std::map<std::string, std::any>{{"cols", std::any(cols)}, {"rows", std::any(rows)}, {"data", std::any(d)}};
}

void setPx(std::map<std::string, std::any> b, int64_t x, int64_t y, Pixel p) {
    int64_t cols = std::any_cast<int64_t>(b[std::string("cols")]);
    int64_t rows = std::any_cast<int64_t>(b[std::string("rows")]);
    if (((((x >= 0) && (x < cols)) && (y >= 0)) && (y < rows))) {
        std::any_cast<std::vector<std::vector<Pixel>>&>(b[std::string("data")])[y][x] = p;
    }
}

void fill(std::map<std::string, std::any> b, Pixel p) {
    int64_t cols = std::any_cast<int64_t>(b[std::string("cols")]);
    int64_t rows = std::any_cast<int64_t>(b[std::string("rows")]);
    int64_t y = 0;
    while ((y < rows)) {
        int64_t x = 0;
        while ((x < cols)) {
            std::any_cast<std::vector<std::vector<Pixel>>&>(b[std::string("data")])[y][x] = p;
            x = (x + 1);
        }
        y = (y + 1);
    }
}

void fillRgb(const std::map<std::string, std::any>& b, int64_t rgb) {
    fill(b, pixelFromRgb(rgb));
}

void line(const std::map<std::string, std::any>& b, int64_t x0, int64_t y0, int64_t x1, int64_t y1, Pixel p) {
    int64_t dx = (x1 - x0);
    if ((dx < 0)) {
        dx = -dx;
    }
    int64_t dy = (y1 - y0);
    if ((dy < 0)) {
        dy = -dy;
    }
    int64_t sx = -1;
    if ((x0 < x1)) {
        sx = 1;
    }
    int64_t sy = -1;
    if ((y0 < y1)) {
        sy = 1;
    }
    int64_t err = (dx - dy);
    while (true) {
        setPx(b, x0, y0, p);
        if (((x0 == x1) && (y0 == y1))) {
            break;
        }
        int64_t e2 = (2 * err);
        if ((e2 > (0 - dy))) {
            err = (err - dy);
            x0 = (x0 + sx);
        }
        if ((e2 < dx)) {
            err = (err + dx);
            y0 = (y0 + sy);
        }
    }
}

void bezier2(const std::map<std::string, std::any>& b, int64_t x1, int64_t y1, int64_t x2, int64_t y2, int64_t x3, int64_t y3, Pixel p) {
    std::vector<int64_t> px = {};
    std::vector<int64_t> py = {};
    int64_t i = 0;
    while ((i <= b2Seg)) {
        px = ([&]{ auto __tmp = px; __tmp.push_back(0); return __tmp; }());
        py = ([&]{ auto __tmp = py; __tmp.push_back(0); return __tmp; }());
        i = (i + 1);
    }
    double fx1 = (double)(x1);
    double fy1 = (double)(y1);
    double fx2 = (double)(x2);
    double fy2 = (double)(y2);
    double fx3 = (double)(x3);
    double fy3 = (double)(y3);
    i = 0;
    while ((i <= b2Seg)) {
        double c = ((double)((double)(i)) / ((double)(b2Seg)));
        double a = (1.0 - c);
        double a2 = (a * a);
        double b2 = ((2.0 * c) * a);
        double c2 = (c * c);
        px[i] = (int64_t)((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)));
        py[i] = (int64_t)((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)));
        i = (i + 1);
    }
    int64_t x0 = px[0];
    int64_t y0 = py[0];
    i = 1;
    while ((i <= b2Seg)) {
        int64_t x = px[i];
        int64_t y = py[i];
        line(b, x0, y0, x, y, p);
        x0 = x;
        y0 = y;
        i = (i + 1);
    }
}

int main() {
    fillRgb(b, 14614575);
    bezier2(b, 20, 150, 500, -100, 300, 280, pixelFromRgb(4165615));
    return 0;
}
