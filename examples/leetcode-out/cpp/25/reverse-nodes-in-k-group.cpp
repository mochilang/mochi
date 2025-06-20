#include <bits/stdc++.h>
using namespace std;

vector<int> reverseKGroup(vector<int> nums, int k){
	auto n = nums.size();
	if (k <= 1) {
		return nums;
	}
	auto result = vector<int>{};
	auto i = 0;
	while (i < n) {
		auto end = i + k;
		if (end <= n) {
			auto j = end - 1;
			while (j >= i) {
				result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{nums[j]});
				j = j - 1;
			}
		} else {
			auto j = i;
			while (j < n) {
				result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{nums[j]});
				j = j + 1;
			}
		}
		i = i + k;
	}
	return result;
}

int main() {
	return 0;
}
