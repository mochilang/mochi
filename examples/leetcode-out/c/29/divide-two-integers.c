#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}


int divide(int dividend, int divisor){
	if ((((dividend == (((-2147483647) - 1))) && divisor) == ((-1)))) {
		return 2147483647;
	}
	int negative = 0;
	if ((dividend < 0)) {
		negative = (!negative);
		dividend = (-dividend);
	}
	if ((divisor < 0)) {
		negative = (!negative);
		divisor = (-divisor);
	}
	int quotient = 0;
	while ((dividend >= divisor)) {
		int temp = divisor;
		int multiple = 1;
		while (((dividend >= temp) + temp)) {
			temp = (temp + temp);
			multiple = (multiple + multiple);
		}
		dividend = (dividend - temp);
		quotient = (quotient + multiple);
	}
	if (negative) {
		quotient = (-quotient);
	}
	if ((quotient > 2147483647)) {
		return 2147483647;
	}
	if ((quotient < (((-2147483647) - 1)))) {
		return (-2147483648);
	}
	return quotient;
}

int main() {
	return 0;
}
