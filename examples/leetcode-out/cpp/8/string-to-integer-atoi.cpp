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

int digit(string ch){
	if (ch == string("0")) {
		return 0;
	}
	if (ch == string("1")) {
		return 1;
	}
	if (ch == string("2")) {
		return 2;
	}
	if (ch == string("3")) {
		return 3;
	}
	if (ch == string("4")) {
		return 4;
	}
	if (ch == string("5")) {
		return 5;
	}
	if (ch == string("6")) {
		return 6;
	}
	if (ch == string("7")) {
		return 7;
	}
	if (ch == string("8")) {
		return 8;
	}
	if (ch == string("9")) {
		return 9;
	}
	return -1;
}

int myAtoi(string s){
	auto i = 0;
	auto n = s.size();
	while (i < n && _indexString(s, i) == _indexString(string(" "), 0)) {
		i = i + 1;
	}
	auto sign = 1;
	if (i < n && (_indexString(s, i) == _indexString(string("+"), 0) || _indexString(s, i) == _indexString(string("-"), 0))) {
		if (_indexString(s, i) == _indexString(string("-"), 0)) {
			sign = -1;
		}
		i = i + 1;
	}
	auto result = 0;
	while (i < n) {
		auto ch = _sliceString(s, i, i + 1);
		auto d = digit(ch);
		if (d < 0) {
			break;
		}
		result = result * 10 + d;
		i = i + 1;
	}
	result = result * sign;
	if (result > 2147483647) {
		return 2147483647;
	}
	if (result < (-2147483648)) {
		return -2147483648;
	}
	return result;
}

int main() {
	return 0;
}
