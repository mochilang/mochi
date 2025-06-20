#include <bits/stdc++.h>
using namespace std;

int removeDuplicates(vector<int> nums){
	if (nums.size() == 0) {
		return 0;
	}
	auto count = 1;
	auto prev = nums[0];
	auto i = 1;
	while (i < nums.size()) {
		auto cur = nums[i];
		if (cur != prev) {
			count = count + 1;
			prev = cur;
		}
		i = i + 1;
	}
	return count;
}

int main() {
	return 0;
}
