#include <bits/stdc++.h>
using namespace std;

double findMedianSortedArrays(vector<int> nums1, vector<int> nums2){
	vector<int> merged = vector<int>{};
	auto i = 0;
	auto j = 0;
	while (i < nums1.size() || j < nums2.size()) {
		if (j >= nums2.size()) {
			merged = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(merged, vector<int>{nums1[i]});
			i = i + 1;
		} else 		if (i >= nums1.size()) {
			merged = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(merged, vector<int>{nums2[j]});
			j = j + 1;
		} else 		if (nums1[i] <= nums2[j]) {
			merged = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(merged, vector<int>{nums1[i]});
			i = i + 1;
		} else {
			merged = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(merged, vector<int>{nums2[j]});
			j = j + 1;
		}
	}
	auto total = merged.size();
	if (total % 2 == 1) {
		return merged[total / 2];
	}
	auto mid1 = merged[total / 2 - 1];
	auto mid2 = merged[total / 2];
	return (mid1 + mid2) / 2.0;
}

int main() {
	return 0;
}
