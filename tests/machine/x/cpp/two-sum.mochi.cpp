#include <bits/stdc++.h>
using namespace std;

auto twoSum(auto nums, auto target) {
	auto n = nums.size();
	for (int i = 0; i < n; ++i) {
		for (int j = (i + 1); j < n; ++j) {
			if (((nums[i] + nums[j]) == target)) {
				return vector<int>{i, j};
			}
		}
	}
	return vector<int>{(-1), (-1)};
}

int main() {
	auto result = twoSum(vector<int>{2, 7, 11, 15}, 9);
	(cout << result[0]);
	(cout << result[1]);
	return 0;
}
