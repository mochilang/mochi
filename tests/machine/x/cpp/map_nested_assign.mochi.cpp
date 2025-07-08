#include <bits/stdc++.h>
using namespace std;

int main() {
	auto data = unordered_map<string,int>{{"outer", unordered_map<string,int>{{"inner", 1}}}};
	data["outer"]["inner"] = 2;
	(cout << data["outer"]["inner"]);
	return 0;
}
