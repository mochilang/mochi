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

int expand(char* s, int left, int right){
	int l = left;
	int r = right;
	int n = strlen(s);
	while ((((l >= 0) && r) < n)) {
		char* _t1 = _index_string(s, l);
		char* _t2 = _index_string(s, r);
		if ((_t1 != _t2)) {
			break;
		}
		l = (l - 1);
		r = (r + 1);
	}
	return ((r - l) - 1);
}

char* longestPalindrome(char* s){
	if ((strlen(s) <= 1)) {
		return s;
	}
	int start = 0;
	int end = 0;
	int n = strlen(s);
	for (int i = 0; i < n; i++) {
		int len1 = expand(s, i, i);
		int len2 = expand(s, i, (i + 1));
		int l = len1;
		if ((len2 > len1)) {
			l = len2;
		}
		if (((l > end) - start)) {
			start = ((i - ((l - 1))) / 2);
			end = ((i + l) / 2);
		}
	}
	char* _t3 = slice_string(s, start, (end + 1));
	return _t3;
}

int main() {
	return 0;
}
