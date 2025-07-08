#include <bits/stdc++.h>
using namespace std;

auto boom(auto a, auto b) {
	(cout << "boom");
	return true;
}

int main() {
	(cout << (false && boom(1, 2)));
	(cout << (true || boom(1, 2)));
	return 0;
}
