#include <bits/stdc++.h>
using namespace std;

auto sum_rec(auto n, auto acc) {
	if ((n == 0)) {
		return acc;
	}
	return sum_rec((n - 1), (acc + n));
}

int main() {
	(cout << sum_rec(10, 0));
	return 0;
}
