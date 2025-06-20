#include <bits/stdc++.h>
using namespace std;

vector<int> mergeTwoLists(vector<int> l1, vector<int> l2){
	auto i = 0;
	auto j = 0;
	auto result = vector<int>{};
	while (i < l1.size() && j < l2.size()) {
		if (l1[i] <= l2[j]) {
			result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{l1[i]});
			i = i + 1;
		} else {
			result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{l2[j]});
			j = j + 1;
		}
	}
	while (i < l1.size()) {
		result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{l1[i]});
		i = i + 1;
	}
	while (j < l2.size()) {
		result = ([&](vector<int> a, vector<int> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(result, vector<int>{l2[j]});
		j = j + 1;
	}
	return result;
}

int main() {
	return 0;
}
