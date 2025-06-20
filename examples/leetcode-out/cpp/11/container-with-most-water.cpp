#include <bits/stdc++.h>
using namespace std;

int maxArea(vector<int> height){
	auto left = 0;
	auto right = height.size() - 1;
	auto maxArea = 0;
	while (left < right) {
		auto width = right - left;
		auto h = 0;
		if (height[left] < height[right]) {
			h = height[left];
		} else {
			h = height[right];
		}
		auto area = h * width;
		if (area > maxArea) {
			maxArea = area;
		}
		if (height[left] < height[right]) {
			left = left + 1;
		} else {
			right = right - 1;
		}
	}
	return maxArea;
}

int main() {
	return 0;
}
