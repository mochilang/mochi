#include <bits/stdc++.h>
using namespace std;

int lengthOfLongestSubstring(string s){
	auto n = s.size();
	auto start = 0;
	auto best = 0;
	auto i = 0;
	while (i < n) {
		auto j = start;
		while (j < i) {
			if (s[j] == s[i]) {
				start = j + 1;
				break;
			}
			j = j + 1;
		}
		auto length = i - start + 1;
		if (length > best) {
			best = length;
		}
		i = i + 1;
	}
	return best;
}

int main() {
	return 0;
}
