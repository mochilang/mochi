#include <bits/stdc++.h>
using namespace std;

vector<int> removeNthFromEnd(vector<int> nums, int n){
	auto idx = nums.size() - n;
	auto result = vector<int>{};
	auto i = 0;
	while (i < nums.size()) {
		if (i != idx) {
			result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{nums[i]});
		}
		i = i + 1;
	}
	return result;
}

int main() {
	return 0;
}
