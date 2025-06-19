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
	if (ch == "0") {
		return 0;
	}
	if (ch == "1") {
		return 1;
	}
	if (ch == "2") {
		return 2;
	}
	if (ch == "3") {
		return 3;
	}
	if (ch == "4") {
		return 4;
	}
	if (ch == "5") {
		return 5;
	}
	if (ch == "6") {
		return 6;
	}
	if (ch == "7") {
		return 7;
	}
	if (ch == "8") {
		return 8;
	}
	if (ch == "9") {
		return 9;
	}
	return -1;
}

int myAtoi(string s){
	int i = 0;
	auto n = s.size();
	while (i < n && _indexString(s, i) == _indexString(" ", 0)) {
		i = i + 1;
	}
	int sign = 1;
	if (i < n && (_indexString(s, i) == _indexString("+", 0) || _indexString(s, i) == _indexString("-", 0))) {
		if (_indexString(s, i) == _indexString("-", 0)) {
			sign = -1;
		}
		i = i + 1;
	}
	int result = 0;
	while (i < n) {
		string ch = _sliceString(s, i, i + 1);
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
