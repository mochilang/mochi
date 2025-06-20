#include <bits/stdc++.h>
using namespace std;

int divide(int dividend, int divisor){
	if (dividend == (-2147483647 - 1) && divisor == (-1)) {
		return 2147483647;
	}
	auto negative = false;
	if (dividend < 0) {
		negative = !negative;
		dividend = -dividend;
	}
	if (divisor < 0) {
		negative = !negative;
		divisor = -divisor;
	}
	auto quotient = 0;
	while (dividend >= divisor) {
		auto temp = divisor;
		auto multiple = 1;
		while (dividend >= temp + temp) {
			temp = temp + temp;
			multiple = multiple + multiple;
		}
		dividend = dividend - temp;
		quotient = quotient + multiple;
	}
	if (negative) {
		quotient = -quotient;
	}
	if (quotient > 2147483647) {
		return 2147483647;
	}
	if (quotient < (-2147483647 - 1)) {
		return -2147483648;
	}
	return quotient;
}

int main() {
	return 0;
}
