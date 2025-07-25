// Generated by Mochi compiler v0.10.25 on 2025-07-13T13:01:39Z
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

template <typename T> void __json(const T &);
inline void __json(int v) { std::cout << v; }
inline void __json(double v) { std::cout << v; }
inline void __json(bool v) { std::cout << (v ? "true" : "false"); }
inline void __json(const std::string &v) { std::cout << "\"" << v << "\""; }
inline void __json(const char *v) { std::cout << "\"" << v << "\""; }
template <typename T> void __json(const std::vector<T> &v) {
  std::cout << "[";
  bool first = true;
  for (const auto &x : v) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(x);
  }
  std::cout << "]";
}
template <typename K, typename V> void __json(const std::map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}
template <typename K, typename V>
void __json(const std::unordered_map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}

struct CastInfo {
  decltype(1) movie_id;
  decltype(101) person_id;
};
struct Keyword {
  decltype(100) id;
  decltype(std::string("marvel-cinematic-universe")) keyword;
};
struct MovieKeyword {
  decltype(1) movie_id;
  decltype(100) keyword_id;
};
struct Name {
  decltype(101) id;
  decltype(std::string("Downey Robert Jr.")) name;
};
struct Title {
  decltype(1) id;
  decltype(std::string("Iron Man 3")) title;
  decltype(2013) production_year;
};
struct Result {
  decltype(k.keyword) movie_keyword;
  decltype(n.name) actor_name;
  decltype(t.title) marvel_movie;
};
inline void __json(const Keyword &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"keyword\":";
  __json(v.keyword);
  std::cout << "}";
}
inline void __json(const Name &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"name\":";
  __json(v.name);
  std::cout << "}";
}
inline void __json(const Title &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"title\":";
  __json(v.title);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"production_year\":";
  __json(v.production_year);
  std::cout << "}";
}
inline void __json(const MovieKeyword &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"movie_id\":";
  __json(v.movie_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"keyword_id\":";
  __json(v.keyword_id);
  std::cout << "}";
}
inline void __json(const CastInfo &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"movie_id\":";
  __json(v.movie_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"person_id\":";
  __json(v.person_id);
  std::cout << "}";
}
inline void __json(const Result &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"movie_keyword\":";
  __json(v.movie_keyword);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"actor_name\":";
  __json(v.actor_name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"marvel_movie\":";
  __json(v.marvel_movie);
  std::cout << "}";
}
int main() {
  std::vector<CastInfo> cast_info = {CastInfo{1, 101}, CastInfo{2, 102}};
  std::vector<Keyword> keyword = {
      Keyword{100, std::string("marvel-cinematic-universe")},
      Keyword{200, std::string("other")}};
  std::vector<MovieKeyword> movie_keyword = {MovieKeyword{1, 100},
                                             MovieKeyword{2, 200}};
  std::vector<Name> name = {Name{101, std::string("Downey Robert Jr.")},
                            Name{102, std::string("Chris Evans")}};
  std::vector<Title> title = {Title{1, std::string("Iron Man 3"), 2013},
                              Title{2, std::string("Old Movie"), 2000}};
  std::vector<Result> result = ([&]() {
    std::vector<Result> __items;
    for (auto ci : cast_info) {
      for (auto mk : movie_keyword) {
        if (!((ci.movie_id == mk.movie_id)))
          continue;
        for (auto k : keyword) {
          if (!((mk.keyword_id == k.id)))
            continue;
          for (auto n : name) {
            if (!((ci.person_id == n.id)))
              continue;
            for (auto t : title) {
              if (!((ci.movie_id == t.id)))
                continue;
              if (!(((((k.keyword ==
                        std::string("marvel-cinematic-universe")) &&
                       (n.name.find(std::string("Downey")) !=
                        std::string::npos)) &&
                      (n.name.find(std::string("Robert")) !=
                       std::string::npos)) &&
                     (t.production_year > 2010))))
                continue;
              __items.push_back(Result{k.keyword, n.name, t.title});
            }
          }
        }
      }
    }
    return __items;
  })();
  (__json(result));
  // test Q6 finds marvel movie with Robert Downey
  return 0;
}
