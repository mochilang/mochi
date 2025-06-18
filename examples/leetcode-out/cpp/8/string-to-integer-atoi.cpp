#include <bits/stdc++.h>
using namespace std;

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
	auto i = 0;
	auto n = s.size();
	while (i < n && s[i] == " "[0]) {
		i = i + 1;
	}
	auto sign = 1;
	if (i < n && (s[i] == "+"[0] || s[i] == "-"[0])) {
		if (s[i] == "-"[0]) {
			sign = -1;
		}
		i = i + 1;
	}
	auto result = 0;
	while (i < n) {
		auto ch = s.substr(i, i + 1 - i);
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
