#include <bits/stdc++.h>
using namespace std;

string convert(string s, int numRows){
	if (numRows <= 1 || numRows >= s.size()) {
		return s;
	}
	vector<string> rows = vector<string>{};
	auto i = 0;
	while (i < numRows) {
		rows = ([&](vector<string> a, vector<string> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(rows, vector<string>{""});
		i = i + 1;
	}
	auto curr = 0;
	auto step = 1;
	for (auto ch : s) {
		rows[curr] = rows[curr] + ch;
		if (curr == 0) {
			step = 1;
		} else 		if (curr == numRows - 1) {
			step = -1;
		}
		curr = curr + step;
	}
	string result = "";
	for (auto row : rows) {
		result = result + row;
	}
	return result;
}

int main() {
	return 0;
}
