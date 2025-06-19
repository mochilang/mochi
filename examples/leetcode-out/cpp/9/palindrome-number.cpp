#include <bits/stdc++.h>
using namespace std;

bool isPalindrome(int x){
	if (x < 0) {
		return false;
	}
	auto s = to_string(x);
	auto n = s.size();
	for (int i = 0; i < n / 2; i++) {
		if (s[i] != s[n - 1 - i]) {
			return false;
		}
	}
	return true;
}

int main() {
	return 0;
}
