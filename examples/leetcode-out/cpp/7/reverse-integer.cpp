#include <bits/stdc++.h>
using namespace std;

int reverse(int x){
	int sign = 1;
	int n = x;
	if (n < 0) {
		sign = -1;
		n = -n;
	}
	int rev = 0;
	while (n != 0) {
		int digit = n % 10;
		rev = rev * 10 + digit;
		n = n / 10;
	}
	rev = rev * sign;
	if (rev < (-2147483647 - 1) || rev > 2147483647) {
		return 0;
	}
	return rev;
}

int main() {
	return 0;
}
