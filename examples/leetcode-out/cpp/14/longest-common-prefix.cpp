#include <bits/stdc++.h>
using namespace std;

string longestCommonPrefix(vector<string> strs){
	if (strs.size() == 0) {
		return string("");
	}
	auto prefix = strs[0];
	for (int i = 1; i < strs.size(); i++) {
		auto j = 0;
		auto current = strs[i];
		while (j < prefix.size() && j < current.size()) {
			if (prefix[j] != current[j]) {
				break;
			}
			j = j + 1;
		}
		prefix = prefix.substr(0, j - 0);
		if (prefix == string("")) {
			break;
		}
	}
	return prefix;
}

int main() {
	return 0;
}
