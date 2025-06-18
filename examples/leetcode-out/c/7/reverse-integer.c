#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}

static int _count(list_int v) {
	return v.len;
}

static double _avg(list_int v) {
	if (v.len == 0) return 0;
	double sum = 0;
	for (int i = 0; i < v.len; i++) {
		sum += v.data[i];
	}
	return sum / v.len;
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
