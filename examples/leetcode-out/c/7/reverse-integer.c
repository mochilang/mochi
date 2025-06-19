#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}


int reverse(int x){
	int sign = 1;
	int n = x;
	if ((n < 0)) {
		sign = (-1);
		n = (-n);
	}
	int rev = 0;
	while ((n != 0)) {
		int digit = (n % 10);
		rev = ((rev * 10) + digit);
		n = (n / 10);
	}
	rev = (rev * sign);
	if ((((rev < (((-2147483647) - 1))) || rev) > 2147483647)) {
		return 0;
	}
	return rev;
}

int main() {
	return 0;
}
