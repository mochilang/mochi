#include <bits/stdc++.h>
using namespace std;

int main() {
	auto x = 3;
	auto y = 4;
	auto m = unordered_map<string,int>{{"a", x}, {"b", y}};
	print(m["a"], m["b"]);
	return 0;
}
