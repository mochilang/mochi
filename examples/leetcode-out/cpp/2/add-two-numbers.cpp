#include <bits/stdc++.h>
using namespace std;

vector<int> addTwoNumbers(vector<int> l1, vector<int> l2){
	auto i = 0;
	auto j = 0;
	auto carry = 0;
	auto result = vector<int>{};
	while (i < l1.size() || j < l2.size() || carry > 0) {
		auto x = 0;
		if (i < l1.size()) {
			x = l1[i];
			i = i + 1;
		}
		auto y = 0;
		if (j < l2.size()) {
			y = l2[j];
			j = j + 1;
		}
		auto sum = x + y + carry;
		auto digit = sum % 10;
		carry = sum / 10;
		result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{digit});
	}
	return result;
}

int main() {
	return 0;
}
