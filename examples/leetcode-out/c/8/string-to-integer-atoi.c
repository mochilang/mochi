#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static char* _index_string(char* s, int i) {
	int len = strlen(s);
	if (i < 0) i += len;
	if (i < 0 || i >= len) { fprintf(stderr, "index out of range\n"); exit(1); }
	char* buf = (char*)malloc(2);
	buf[0] = s[i];
	buf[1] = '\0';
	return buf;
}

int myAtoi(char* s){
	int i = 0;
	int n = strlen(s);
	char* _t1 = _index_string(s, i);
	while ((((i < n) && _t1) == " ")) {
		i = (i + 1);
	}
	int sign = 1;
	char* _t2 = _index_string(s, i);
	char* _t3 = _index_string(s, i);
	if (((i < n) && ((((_t2 == "+") || _t3) == "-")))) {
		char* _t4 = _index_string(s, i);
		if ((_t4 == "-")) {
			sign = (-1);
		}
		i = (i + 1);
	}
	int digits = 0;
	int result = 0;
	while ((i < n)) {
		char* _t5 = _index_string(s, i);
		int ch = _t5;
		if ((!((ch in digits)))) {
			break;
		}
		int d = digits.data[ch];
		result = ((result * 10) + d);
		i = (i + 1);
	}
	result = (result * sign);
	if ((result > 2147483647)) {
		return 2147483647;
	}
	if ((result < ((-2147483648)))) {
		return (-2147483648);
	}
	return result;
}

int main() {
	return 0;
}
