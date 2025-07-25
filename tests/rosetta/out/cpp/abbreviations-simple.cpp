// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
#include <any>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

inline bool __any_eq(const std::any &a, const std::any &b) {
  if (a.type() != b.type())
    return false;
  if (a.type() == typeid(int))
    return std::any_cast<int>(a) == std::any_cast<int>(b);
  if (a.type() == typeid(double))
    return std::any_cast<double>(a) == std::any_cast<double>(b);
  if (a.type() == typeid(bool))
    return std::any_cast<bool>(a) == std::any_cast<bool>(b);
  if (a.type() == typeid(std::string))
    return std::any_cast<std::string>(a) == std::any_cast<std::string>(b);
  return false;
}
inline void __print_any(const std::any &a) {
  if (a.type() == typeid(int))
    std::cout << std::any_cast<int>(a);
  else if (a.type() == typeid(double))
    std::cout << std::any_cast<double>(a);
  else if (a.type() == typeid(bool))
    std::cout << (std::any_cast<bool>(a) ? "true" : "false");
  else if (a.type() == typeid(std::string))
    std::cout << std::any_cast<std::string>(a);
}
inline std::string __any_str(const std::any &a) {
  if (a.type() == typeid(int))
    return std::to_string(std::any_cast<int>(a));
  if (a.type() == typeid(double))
    return std::to_string(std::any_cast<double>(a));
  if (a.type() == typeid(bool))
    return std::any_cast<bool>(a) ? "true" : "false";
  if (a.type() == typeid(std::string))
    return std::any_cast<std::string>(a);
  return "";
}

std::vector<std::string> fields(std::string s) {
  std::vector<std::string> words = std::vector<>{};
  auto cur = std::string("");
  auto i = 0;
  while ((i < s.size())) {
    auto ch = std::string(s).substr(i, ((i + 1)) - (i));
    if ((((ch == std::string(" ")) || (ch == std::string("\n"))) ||
         (ch == std::string("\t")))) {
      if ((cur.size() > 0)) {
        words.push_back(cur);
        cur = std::string("");
      }
    } else {
      cur = (cur + ch);
    }
    i = (i + 1);
  }
  if ((cur.size() > 0)) {
    words.push_back(cur);
  }
  return words;
}

std::string padRight(std::string s, int width) {
  auto out = s;
  auto i = s.size();
  while ((i < width)) {
    out = (out + std::string(" "));
    i = (i + 1);
  }
  return out;
}

std::string join(std::vector<std::string> xs, std::string sep) {
  auto res = std::string("");
  auto i = 0;
  while ((i < xs.size())) {
    if ((i > 0)) {
      res = (res + sep);
    }
    res = (res + xs[i]);
    i = (i + 1);
  }
  return res;
}

int parseIntStr(std::string str) {
  auto i = 0;
  auto neg = false;
  if (((str.size() > 0) &&
       (std::string(str).substr(0, (1) - (0)) == std::string("-")))) {
    neg = true;
    i = 1;
  }
  auto n = 0;
  auto digits = std::unordered_map<std::string, int>{
      {std::string("0"), 0}, {std::string("1"), 1}, {std::string("2"), 2},
      {std::string("3"), 3}, {std::string("4"), 4}, {std::string("5"), 5},
      {std::string("6"), 6}, {std::string("7"), 7}, {std::string("8"), 8},
      {std::string("9"), 9}};
  while ((i < str.size())) {
    n = ((n * 10) + digits[std::string(str).substr(i, ((i + 1)) - (i))]);
    i = (i + 1);
  }
  if (neg) {
    n = (-n);
  }
  return n;
}

bool isDigits(std::string s) {
  if ((s.size() == 0)) {
    return false;
  }
  auto i = 0;
  while ((i < s.size())) {
    auto ch = std::string(s).substr(i, ((i + 1)) - (i));
    if (((ch < std::string("0")) || (ch > std::string("9")))) {
      return false;
    }
    i = (i + 1);
  }
  return true;
}

auto readTable(std::string table) {
  auto toks = fields(table);
  std::vector<std::string> cmds = std::vector<>{};
  std::vector<int> mins = std::vector<>{};
  auto i = 0;
  while ((i < toks.size())) {
    auto cmd = toks[i];
    auto minlen = cmd.size();
    i = (i + 1);
    if (((i < toks.size()) && isDigits(toks[i]))) {
      auto num = parseIntStr(toks[i]);
      if (((num >= 1) && (num < cmd.size()))) {
        minlen = num;
        i = (i + 1);
      }
    }
    cmds.push_back(cmd);
    mins.push_back(minlen);
  }
  return std::unordered_map<std::string, std::any>{
      {std::string("commands"), cmds}, {std::string("mins"), mins}};
}

std::vector<std::string> validate(std::vector<std::string> commands,
                                  std::vector<int> mins,
                                  std::vector<std::string> words) {
  std::vector<std::string> results = std::vector<>{};
  auto wi = 0;
  while ((wi < words.size())) {
    std::vector<std::string> w = words[wi];
    auto found = false;
    auto wlen = w.size();
    auto ci = 0;
    while ((ci < commands.size())) {
      std::vector<std::string> cmd = commands[ci];
      if ((((mins[ci] != 0) && (wlen >= mins[ci])) && (wlen <= cmd.size()))) {
        auto c = upper(cmd);
        auto ww = upper(w);
        if ((std::string(c).substr(0, (wlen) - (0)) == ww)) {
          results.push_back(c);
          found = true;
          break;
        }
      }
      ci = (ci + 1);
    }
    if ((!found)) {
      results.push_back(std::string("*error*"));
    }
    wi = (wi + 1);
  }
  return results;
}

auto __mochi_main() {
  auto table = std::string(""
                           "add 1  alter 3  backup 2  bottom 1  Cappend 2  "
                           "change 1  Schange  Cinsert 2  Clast 3 "
                           "compress 4 copy 2 count 3 Coverlay 3 cursor 3  "
                           "delete 3 Cdelete 2  down 1  duplicate "
                           "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 "
                           "Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 "
                           "forward 2  get  help 1 hexType 4  input 1 "
                           "powerInput 3  join 1 split 2 spltJOIN load "
                           "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix "
                           "2  macro  merge 2 modify 3 move 2 "
                           "msg  next 1 overlay 1 parse preserve 4 purge 3 put "
                           "putD query 1 quit  read recover 3 "
                           "refresh renum 3 repeat 3 replace 1 Creplace 2 "
                           "reset 3 restore 4 rgtLEFT right 2 left "
                           "2  save  set  shift 2  si  sort  sos  stack 3 "
                           "status 4 top  transfer 3  type 1  up 1 ");
  auto sentence = std::string(
      "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin");
  auto tbl = readTable(table);
  std::vector<std::any> commands = tbl[std::string("commands")];
  std::vector<std::any> mins = tbl[std::string("mins")];
  auto words = fields(sentence);
  auto results = validate(commands, mins, words);
  auto out1 = std::string("user words:");
  auto k = 0;
  while ((k < words.size())) {
    out1 = (out1 + std::string(" "));
    if ((k < (words.size() - 1))) {
      out1 = (out1 + padRight(words[k], results[k].size()));
    } else {
      out1 = (out1 + words[k]);
    }
    k = (k + 1);
  }
  std::cout << out1 << std::endl;
  std::cout << (std::string("full words: ") + join(results, std::string(" ")))
            << std::endl;
}

int main() {
  __mochi_main();
  return 0;
}
