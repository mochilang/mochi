// Generated by Mochi compiler v0.10.25 on 2025-07-13T13:01:38Z
#include <algorithm>
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

struct CompanyType {
  decltype(1) ct_id;
  decltype(std::string("production companies")) kind;
};
struct InfoType {
  decltype(10) it_id;
  decltype(std::string("languages")) info;
};
struct Title {
  decltype(100) t_id;
  decltype(std::string("B Movie")) title;
  decltype(2010) production_year;
};
struct MovieCompany {
  decltype(100) movie_id;
  decltype(1) company_type_id;
  decltype(std::string("ACME (France) (theatrical)")) note;
};
struct MovieInfo {
  decltype(100) movie_id;
  decltype(std::string("German")) info;
  decltype(10) info_type_id;
};
inline void __json(const CompanyType &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ct_id\":";
  __json(v.ct_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"kind\":";
  __json(v.kind);
  std::cout << "}";
}
inline void __json(const InfoType &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"it_id\":";
  __json(v.it_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"info\":";
  __json(v.info);
  std::cout << "}";
}
inline void __json(const MovieCompany &v) {
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
  std::cout << "\"company_type_id\":";
  __json(v.company_type_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"note\":";
  __json(v.note);
  std::cout << "}";
}
inline void __json(const MovieInfo &v) {
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
  std::cout << "\"info\":";
  __json(v.info);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"info_type_id\":";
  __json(v.info_type_id);
  std::cout << "}";
}
inline void __json(const Title &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"t_id\":";
  __json(v.t_id);
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
int main() {
  std::vector<CompanyType> company_type = {
      CompanyType{1, std::string("production companies")},
      CompanyType{2, std::string("other")}};
  std::vector<InfoType> info_type = {InfoType{10, std::string("languages")}};
  std::vector<Title> title = {Title{100, std::string("B Movie"), 2010},
                              Title{200, std::string("A Film"), 2012},
                              Title{300, std::string("Old Movie"), 2000}};
  std::vector<MovieCompany> movie_companies = {
      MovieCompany{100, 1, std::string("ACME (France) (theatrical)")},
      MovieCompany{200, 1, std::string("ACME (France) (theatrical)")},
      MovieCompany{300, 1, std::string("ACME (France) (theatrical)")}};
  std::vector<MovieInfo> movie_info = {
      MovieInfo{100, std::string("German"), 10},
      MovieInfo{200, std::string("Swedish"), 10},
      MovieInfo{300, std::string("German"), 10}};
  auto candidate_titles = ([&]() {
    std::vector<decltype(t.title)> __items;
    for (auto ct : company_type) {
      for (auto mc : movie_companies) {
        if (!((mc.company_type_id == ct.ct_id)))
          continue;
        for (auto mi : movie_info) {
          if (!((mi.movie_id == mc.movie_id)))
            continue;
          for (auto it : info_type) {
            if (!((it.it_id == mi.info_type_id)))
              continue;
            for (auto t : title) {
              if (!((t.t_id == mc.movie_id)))
                continue;
              if (!((((((ct.kind == std::string("production companies")) &&
                        (std::find(mc.note.begin(), mc.note.end(),
                                   std::string("(theatrical)")) !=
                         mc.note.end())) &&
                       (std::find(mc.note.begin(), mc.note.end(),
                                  std::string("(France)")) != mc.note.end())) &&
                      (t.production_year > 2005)) &&
                     ((std::find(
                           std::vector<std::string>{
                               std::string("Sweden"), std::string("Norway"),
                               std::string("Germany"), std::string("Denmark"),
                               std::string("Swedish"), std::string("Denish"),
                               std::string("Norwegian"), std::string("German")}
                               .begin(),
                           std::vector<std::string>{
                               std::string("Sweden"), std::string("Norway"),
                               std::string("Germany"), std::string("Denmark"),
                               std::string("Swedish"), std::string("Denish"),
                               std::string("Norwegian"), std::string("German")}
                               .end(),
                           mi.info) !=
                       std::vector<std::string>{
                           std::string("Sweden"), std::string("Norway"),
                           std::string("Germany"), std::string("Denmark"),
                           std::string("Swedish"), std::string("Denish"),
                           std::string("Norwegian"), std::string("German")}
                           .end())))))
                continue;
              __items.push_back(t.title);
            }
          }
        }
      }
    }
    return __items;
  })();
  std::vector<std::string> result = std::vector<
      decltype(std::unordered_map<std::string, decltype((*std::min_element(
                                                   candidate_titles.begin(),
                                                   candidate_titles.end())))>{
          {std::string("typical_european_movie"),
           (*std::min_element(candidate_titles.begin(),
                              candidate_titles.end()))}})>{
      std::unordered_map<std::string,
                         decltype((*std::min_element(candidate_titles.begin(),
                                                     candidate_titles.end())))>{
          {std::string("typical_european_movie"),
           (*std::min_element(candidate_titles.begin(),
                              candidate_titles.end()))}}};
  (__json(result));
  // test Q5 finds the lexicographically first qualifying title
  return 0;
}
