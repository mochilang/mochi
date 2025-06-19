#include <bits/stdc++.h>
using namespace std;

string convert(string s, int numRows){
	if (numRows <= 1 || numRows >= s.size()) {
		return s;
	}
	vector<string> rows = vector<string>{};
	int i = 0;
	while (i < numRows) {
		rows = ([&](vector<string> a, vector<string> b){ a.insert(a.end(), b.begin(), b.end()); return a; })(rows, vector<string>{string("")});
		i = i + 1;
	}
	int curr = 0;
	int step = 1;
	for (char ch : s) {
		rows[curr] = rows[curr] + string(1, ch);
		if (curr == 0) {
			step = 1;
		} else 		if (curr == numRows - 1) {
			step = -1;
		}
		curr = curr + step;
	}
	string result = string("");
	for (const string& row : rows) {
		result = result + string(row);
	}
	return result;
}

int main() {
	return 0;
}
