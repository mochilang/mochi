#include <bits/stdc++.h>
using namespace std;

int main() {
	(cout << (vector<int>{1, 2} union vector<int>{2, 3}));
	(cout << (vector<int>{1, 2, 3} except vector<int>{2}));
	(cout << (vector<int>{1, 2, 3} intersect vector<int>{2, 4}));
	(cout << (vector<int>{1, 2} union vector<int>{2, 3}).size());
	return 0;
}
