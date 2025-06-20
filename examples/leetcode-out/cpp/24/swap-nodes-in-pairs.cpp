#include <bits/stdc++.h>
using namespace std;

vector<int> swapPairs(vector<int> nums){
	auto i = 0;
	auto result = vector<int>{};
	while (i < nums.size()) {
		if (i + 1 < nums.size()) {
			result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{nums[i + 1], nums[i]});
		} else {
			result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{nums[i]});
		}
		i = i + 2;
	}
	return result;
}

int main() {
	return 0;
}
