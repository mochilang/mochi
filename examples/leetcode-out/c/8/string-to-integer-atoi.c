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


static char* _index_string(char* s, int i) {
	int len = strlen(s);
	if (i < 0) i += len;
	if (i < 0 || i >= len) { fprintf(stderr, "index out of range\n"); exit(1); }
	char* buf = (char*)malloc(2);
	buf[0] = s[i];
	buf[1] = '\0';
	return buf;
}

static char* slice_string(char* s, int start, int end) {
	int len = strlen(s);
	if (start < 0) start += len;
	if (end < 0) end += len;
	if (start < 0) start = 0;
	if (end > len) end = len;
	if (start > end) start = end;
	char* buf = (char*)malloc(end - start + 1);
	memcpy(buf, s + start, end - start);
	buf[end - start] = '\0';
	return buf;
}

int digit(char* ch){
	if ((ch == "0")) {
		return 0;
	}
	if ((ch == "1")) {
		return 1;
	}
	if ((ch == "2")) {
		return 2;
	}
	if ((ch == "3")) {
		return 3;
	}
	if ((ch == "4")) {
		return 4;
	}
	if ((ch == "5")) {
		return 5;
	}
	if ((ch == "6")) {
		return 6;
	}
	if ((ch == "7")) {
		return 7;
	}
	if ((ch == "8")) {
		return 8;
	}
	if ((ch == "9")) {
		return 9;
	}
	return (-1);
}

int myAtoi(char* s){
	int i = 0;
	int n = strlen(s);
	char* _t1 = _index_string(s, i);
	char* _t2 = _index_string(" ", 0);
	while ((((i < n) && _t1) == _t2)) {
		i = (i + 1);
	}
	int sign = 1;
	char* _t3 = _index_string(s, i);
	char* _t4 = _index_string("+", 0);
	char* _t5 = _index_string(s, i);
	char* _t6 = _index_string("-", 0);
	if (((i < n) && ((((_t3 == _t4) || _t5) == _t6)))) {
		char* _t7 = _index_string(s, i);
		char* _t8 = _index_string("-", 0);
		if ((_t7 == _t8)) {
			sign = (-1);
		}
		i = (i + 1);
	}
	int result = 0;
	while ((i < n)) {
		char* _t9 = slice_string(s, i, (i + 1));
		char* ch = _t9;
		int d = digit(ch);
		if ((d < 0)) {
			break;
		}
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
