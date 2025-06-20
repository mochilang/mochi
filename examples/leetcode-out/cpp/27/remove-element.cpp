#include <bits/stdc++.h>
using namespace std;

int removeElement(vector<int> nums, int val){
	auto k = 0;
	auto i = 0;
	while (i < nums.size()) {
		if (nums[i] != val) {
			nums[k] = nums[i];
			k = k + 1;
		}
		i = i + 1;
	}
	return k;
}

int main() {
	return 0;
}
