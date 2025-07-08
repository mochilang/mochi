#include <bits/stdc++.h>
using namespace std;

int main() {
	auto m = unordered_map<string,int>{{"a", 1}, {"b", 2}};
	for (auto k : m) {
		(cout << k);
	}
	return 0;
}
