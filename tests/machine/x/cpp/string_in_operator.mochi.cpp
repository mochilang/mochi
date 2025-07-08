#include <bits/stdc++.h>
using namespace std;

int main() {
	auto s = "catch";
	(cout << (find("cat".begin(), "cat".end(), s) != "cat".end()));
	(cout << (find("dog".begin(), "dog".end(), s) != "dog".end()));
	return 0;
}
