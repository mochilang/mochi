#include <bits/stdc++.h>
using namespace std;

int main() {
	auto m = unordered_map<string,int>{{1, "a"}, {2, "b"}};
	(cout << (find(1.begin(), 1.end(), m) != 1.end()));
	(cout << (find(3.begin(), 3.end(), m) != 3.end()));
	return 0;
}
