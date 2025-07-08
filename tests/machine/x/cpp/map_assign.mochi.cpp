#include <bits/stdc++.h>
using namespace std;

int main() {
	auto scores = unordered_map<string,int>{{"alice", 1}};
	scores["bob"] = 2;
	(cout << scores["bob"]);
	return 0;
}
