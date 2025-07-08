#include <bits/stdc++.h>
using namespace std;

int main() {
	auto matrix = vector<int>{vector<int>{1, 2}, vector<int>{3, 4}};
	matrix[1][0] = 5;
	(cout << matrix[1][0]);
	return 0;
}
