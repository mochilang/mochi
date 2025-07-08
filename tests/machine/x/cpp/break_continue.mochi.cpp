#include <bits/stdc++.h>
using namespace std;

int main() {
	auto numbers = vector<int>{1, 2, 3, 4, 5, 6, 7, 8, 9};
	for (auto n : numbers) {
		if (((n % 2) == 0)) {
			continue;
		}
		if ((n > 7)) {
			break;
		}
		print("odd number:", n);
	}
	return 0;
}
