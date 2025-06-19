#include <bits/stdc++.h>
using namespace std;

string _indexString(const string& s, int i) {
	int n = s.size();
	if (i < 0) i += n;
	if (i < 0 || i >= n) throw std::out_of_range("index out of range");
	return string(1, s[i]);
}

bool isPalindrome(int x){
	if (x < 0) {
		return false;
	}
	string s = to_string(x);
	auto n = s.size();
	for (int i = 0; i < n / 2; i++) {
		if (_indexString(s, i) != _indexString(s, n - 1 - i)) {
			return false;
		}
	}
	return true;
}

int main() {
	return 0;
}
