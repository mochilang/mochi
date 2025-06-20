#include <bits/stdc++.h>
using namespace std;

vector<int> mergeKLists(vector<vector<int>> lists){
	auto k = lists.size();
	vector<int> indices = vector<int>{};
	auto i = 0;
	while (i < k) {
		indices = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(indices, vector<int>{0});
		i = i + 1;
	}
	vector<int> result = vector<int>{};
	while (true) {
		auto best = 0;
		auto bestList = -1;
		auto found = false;
		auto j = 0;
		while (j < k) {
			auto idx = indices[j];
			if (idx < lists[j].size()) {
				auto val = lists[j][idx];
				if (!found || val < best) {
					best = val;
					bestList = j;
					found = true;
				}
			}
			j = j + 1;
		}
		if (!found) {
			break;
		}
		result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{best});
		indices[bestList] = indices[bestList] + 1;
	}
	return result;
}

int main() {
	return 0;
}
