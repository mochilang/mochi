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

static char* _str(int v) {
	char* buf = (char*)malloc(32);
	sprintf(buf, "%d", v);
	return buf;
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

int isPalindrome(int x){
	if ((x < 0)) {
		return 0;
	}
	char* _t1 = _str(x);
	int s = _t1;
	int n = strlen(s);
	for (int i = 0; i < (n / 2); i++) {
		char* _t2 = _index_string(s, i);
		char* _t3 = _index_string(s, ((n - 1) - i));
		if ((_t2 != _t3)) {
			return 0;
		}
	}
	return 1;
}

int main() {
	return 0;
}
