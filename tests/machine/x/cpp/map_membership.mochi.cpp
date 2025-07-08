#include <bits/stdc++.h>
using namespace std;

int main() {
	auto m = unordered_map<string,int>{{"a", 1}, {"b", 2}};
	(cout << (find("a".begin(), "a".end(), m) != "a".end()));
	(cout << (find("c".begin(), "c".end(), m) != "c".end()));
	return 0;
}
