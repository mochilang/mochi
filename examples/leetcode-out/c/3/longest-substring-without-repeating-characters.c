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

int lengthOfLongestSubstring(char* s){
	int n = strlen(s);
	int start = 0;
	int best = 0;
	int i = 0;
	while ((i < n)) {
		int j = start;
		while ((j < i)) {
			char* _t1 = _index_string(s, j);
			char* _t2 = _index_string(s, i);
			if ((_t1 == _t2)) {
				start = (j + 1);
				break;
			}
			j = (j + 1);
		}
		int length = ((i - start) + 1);
		if ((length > best)) {
			best = length;
		}
		i = (i + 1);
	}
	return best;
}

int main() {
	return 0;
}
