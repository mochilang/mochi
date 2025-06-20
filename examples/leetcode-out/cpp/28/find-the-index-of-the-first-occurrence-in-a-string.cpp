#include <bits/stdc++.h>
using namespace std;

string _indexString(const string& s, int i) {
	int n = s.size();
	if (i < 0) i += n;
	if (i < 0 || i >= n) throw std::out_of_range("index out of range");
	return string(1, s[i]);
}

int strStr(string haystack, string needle){
	auto n = haystack.size();
	auto m = needle.size();
	if (m == 0) {
		return 0;
	}
	if (m > n) {
		return -1;
	}
	for (int i = 0; i < n - m + 1; i++) {
		auto j = 0;
		while (j < m) {
			if (_indexString(haystack, i + j) != _indexString(needle, j)) {
				break;
			}
			j = j + 1;
		}
		if (j == m) {
			return i;
		}
	}
	return -1;
}

int main() {
	return 0;
}
