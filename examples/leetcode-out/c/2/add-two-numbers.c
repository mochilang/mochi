#include <stdio.h>
#include <stdlib.h>

typedef struct { int len; int *data; } list_int;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}

static list_int concat_list_int(list_int a, list_int b) {
	list_int r = list_int_create(a.len + b.len);
	for (int i = 0; i < a.len; i++) {
		r.data[i] = a.data[i];
	}
	for (int i = 0; i < b.len; i++) {
		r.data[a.len + i] = b.data[i];
	}
	return r;
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

list_int addTwoNumbers(list_int l1, list_int l2){
	int i = 0;
	int j = 0;
	int carry = 0;
	list_int _t1 = list_int_create(0);
	list_int result = _t1;
	while ((((((i < l1.len) || j) < l2.len) || carry) > 0)) {
		int x = 0;
		if ((i < l1.len)) {
			x = l1.data[i];
			i = (i + 1);
		}
		int y = 0;
		if ((j < l2.len)) {
			y = l2.data[j];
			j = (j + 1);
		}
		int sum = ((x + y) + carry);
		int digit = (sum % 10);
		carry = (sum / 10);
		list_int _t2 = list_int_create(1);
		_t2.data[0] = digit;
		list_int _t3 = concat_list_int(result, _t2);
		result = _t3;
	}
	return result;
}

int main() {
	return 0;
}
