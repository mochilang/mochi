// Generated by Mochi transpiler v0.10.52 on 2025-08-01 17:48:14 GMT+7
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <optional>
#include <any>
#include <type_traits>
#include <cstdlib>
#include <chrono>
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
static std::string _repeat(const std::string& s, int64_t n) {
    std::string out; out.reserve(s.size()*n);
    for(int64_t i=0;i<n;i++) out += s;
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

std::string repeat(std::string ch, int64_t n);
bool chance(double prob);
std::vector<std::vector<std::string>> newBoard();
std::vector<std::vector<std::string>> step(const std::vector<std::vector<std::string>>& src);
void printBoard(const std::vector<std::vector<std::string>>& b);
int main();

int64_t rows = int64_t(20);
int64_t cols = int64_t(30);
double p = 0.01;
double f = 0.001;
std::vector<std::vector<std::string>> board = newBoard();

std::string repeat(std::string ch, int64_t n) {
    std::string s = std::string("");
    int64_t i = int64_t(0);
    while ((i < n)) {
        s = (s + ch);
        i = (i + int64_t(1));
    }
    return s;
}

bool chance(double prob) {
    int64_t threshold = (int64_t)((prob * 1000.0));
    return ((_now() % int64_t(1000)) < threshold);
}

std::vector<std::vector<std::string>> newBoard() {
    std::vector<std::vector<std::string>> b = {};
    int64_t r = int64_t(0);
    while ((r < rows)) {
        std::vector<std::string> row = {};
        int64_t c = int64_t(0);
        while ((c < cols)) {
            if (((_now() % int64_t(2)) == int64_t(0))) {
                row = ([&]{ auto __tmp = row; __tmp.push_back(std::string("T")); return __tmp; }());
            } else {
                row = ([&]{ auto __tmp = row; __tmp.push_back(std::string(" ")); return __tmp; }());
            }
            c = (c + int64_t(1));
        }
        b = ([&]{ auto __tmp = b; __tmp.push_back(row); return __tmp; }());
        r = (r + int64_t(1));
    }
    return b;
}

std::vector<std::vector<std::string>> step(const std::vector<std::vector<std::string>>& src) {
    std::vector<std::vector<std::string>> dst = {};
    int64_t r = int64_t(0);
    while ((r < rows)) {
        std::vector<std::string> row = {};
        int64_t c = int64_t(0);
        while ((c < cols)) {
            std::string cell = src[r][c];
            std::string next = cell;
            if ((cell == std::string("#"))) {
                next = std::string(" ");
            } else             if ((cell == std::string("T"))) {
                bool burning = false;
                int64_t dr = -int64_t(1);
                while ((dr <= int64_t(1))) {
                    int64_t dc = -int64_t(1);
                    while ((dc <= int64_t(1))) {
                        if (((dr != int64_t(0)) || (dc != int64_t(0)))) {
                            int64_t rr = (r + dr);
                            int64_t cc = (c + dc);
                            if (((((rr >= int64_t(0)) && (rr < rows)) && (cc >= int64_t(0))) && (cc < cols))) {
                                if ((src[rr][cc] == std::string("#"))) {
                                    burning = true;
                                }
                            }
                        }
                        dc = (dc + int64_t(1));
                    }
                    dr = (dr + int64_t(1));
                }
                if ((burning || chance(f))) {
                    next = std::string("#");
                }
            } else {
                if (chance(p)) {
                    next = std::string("T");
                }
            }

            row = ([&]{ auto __tmp = row; __tmp.push_back(next); return __tmp; }());
            c = (c + int64_t(1));
        }
        dst = ([&]{ auto __tmp = dst; __tmp.push_back(row); return __tmp; }());
        r = (r + int64_t(1));
    }
    return dst;
}

void printBoard(const std::vector<std::vector<std::string>>& b) {
    std::cout << (_repeat(std::string("__"), cols) + std::string("\n\n"));
    std::cout << std::endl;
    int64_t r = int64_t(0);
    while ((r < rows)) {
        std::string line = std::string("");
        int64_t c = int64_t(0);
        while ((c < cols)) {
            std::string cell = b[r][c];
            if ((cell == std::string(" "))) {
                line = (line + std::string("  "));
            } else {
                line = ((line + std::string(" ")) + cell);
            }
            c = (c + int64_t(1));
        }
        std::cout << (line + std::string("\n"));
        std::cout << std::endl;
        r = (r + int64_t(1));
    }
}

int main() {
    printBoard(board);
    board = step(board);
    printBoard(board);
    return 0;
}
