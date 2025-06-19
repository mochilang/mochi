#include <bits/stdc++.h>
using namespace std;

string _indexString(const string& s, int i) {
	int n = s.size();
	if (i < 0) i += n;
	if (i < 0 || i >= n) throw std::out_of_range("index out of range");
	return string(1, s[i]);
}

int lengthOfLongestSubstring(string s){
	auto n = s.size();
	int start = 0;
	int best = 0;
	int i = 0;
	while (i < n) {
		int j = start;
		while (j < i) {
			if (_indexString(s, j) == _indexString(s, i)) {
				start = j + 1;
				break;
			}
			j = j + 1;
		}
		int length = i - start + 1;
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
