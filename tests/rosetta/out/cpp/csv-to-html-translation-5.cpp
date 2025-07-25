// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:14Z
#include <iostream>
#include <string>
#include <vector>

auto split(std::string s, std::string sep) {
  auto out = std::vector<int>{};
  auto start = 0;
  auto i = 0;
  auto n = sep.size();
  while ((i <= (s.size() - n))) {
    if ((std::string(s).substr(i, ((i + n)) - (i)) == sep)) {
      out.push_back(std::string(s).substr(start, (i) - (start)));
      i = (i + n);
      start = i;
    } else {
      i = (i + 1);
    }
  }
  out.push_back(std::string(s).substr(start, (s.size()) - (start)));
  return out;
}

std::string htmlEscape(std::string s) {
  auto out = std::string("");
  auto i = 0;
  while ((i < s.size())) {
    auto ch = std::string(s).substr(i, ((i + 1)) - (i));
    if ((ch == std::string("&"))) {
      out = (out + std::string("&amp;"));
    }
    i = (i + 1);
  }
  return out;
}

int main() {
  auto c =
      std::string("Character,Speech\n"
                  "The multitude,The messiah! Show us the messiah!\n"
                  "Brians mother,<angry>Now you listen here! He's not the "
                  "messiah; he's a very naughty boy! Now go away!</angry>\n"
                  "The multitude,Who are you?\n"
                  "Brians mother,I'm his mother; that's who!\n"
                  "The multitude,Behold his mother! Behold his mother!");
  auto rows = std::vector<int>{};
  for (auto line : split(c, std::string("\n"))) {
    rows.push_back(split(line, std::string(",")));
  }
  std::cout << std::string("<table>") << std::endl;
  for (auto row : rows) {
    auto cells = std::string("");
    for (auto cell : row) {
      cells = (((cells + std::string("<td>")) + htmlEscape(cell)) +
               std::string("</td>"));
    }
    std::cout << ((std::string("    <tr>") + cells) + std::string("</tr>"))
              << std::endl;
  }
  std::cout << std::string("</table>") << std::endl;
  return 0;
}
