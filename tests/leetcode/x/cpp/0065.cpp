#include <iostream>
#include <string>
using namespace std;

bool isNumber(const string& s) {
  bool seenDigit = false, seenDot = false, seenExp = false, digitAfterExp = true;
  for (int i = 0; i < (int)s.size(); i++) {
    char ch = s[i];
    if (ch >= '0' && ch <= '9') {
      seenDigit = true;
      if (seenExp) digitAfterExp = true;
    } else if (ch == '+' || ch == '-') {
      if (i != 0 && s[i - 1] != 'e' && s[i - 1] != 'E') return false;
    } else if (ch == '.') {
      if (seenDot || seenExp) return false;
      seenDot = true;
    } else if (ch == 'e' || ch == 'E') {
      if (seenExp || !seenDigit) return false;
      seenExp = true;
      digitAfterExp = false;
    } else return false;
  }
  return seenDigit && digitAfterExp;
}

int main() {
  int t; string s;
  if (!(cin >> t)) return 0;
  getline(cin, s);
  for (int i = 0; i < t; i++) {
    getline(cin, s);
    cout << (isNumber(s) ? "true" : "false");
    if (i + 1 < t) cout << '\n';
  }
}
