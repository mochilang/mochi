#include <bits/stdc++.h>
using namespace std;

string _indexString(const string& s, int i) {
	int n = s.size();
	if (i < 0) i += n;
	if (i < 0 || i >= n) throw std::out_of_range("index out of range");
	return string(1, s[i]);
}

string _sliceString(const string& s, int start, int end) {
	int n = s.size();
	if (start < 0) start += n;
	if (end < 0) end += n;
	if (start < 0) start = 0;
	if (end > n) end = n;
	if (end < start) end = start;
	return s.substr(start, end - start);
}

int expand(string s, int left, int right){
	int l = left;
	int r = right;
	auto n = s.size();
	while (l >= 0 && r < n) {
		if (_indexString(s, l) != _indexString(s, r)) {
			break;
		}
		l = l - 1;
		r = r + 1;
	}
	return r - l - 1;
}

string longestPalindrome(string s){
	if (s.size() <= 1) {
		return s;
	}
	int start = 0;
	int end = 0;
	auto n = s.size();
	for (int i = 0; i < n; i++) {
		auto len1 = expand(s, i, i);
		auto len2 = expand(s, i, i + 1);
		auto l = len1;
		if (len2 > len1) {
			l = len2;
		}
		if (l > (end - start)) {
			start = i - ((l - 1) / 2);
			end = i + (l / 2);
		}
	}
	return _sliceString(s, start, end + 1);
}

int main() {
	return 0;
}
