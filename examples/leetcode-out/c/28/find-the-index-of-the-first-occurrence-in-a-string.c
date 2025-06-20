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

int strStr(char* haystack, char* needle){
	int n = strlen(haystack);
	int m = strlen(needle);
	if ((m == 0)) {
		return 0;
	}
	if ((m > n)) {
		return (-1);
	}
	for (int i = 0; i < ((n - m) + 1); i++) {
		int j = 0;
		while ((j < m)) {
			char* _t1 = _index_string(haystack, (i + j));
			char* _t2 = _index_string(needle, j);
			if ((_t1 != _t2)) {
				break;
			}
			j = (j + 1);
		}
		if ((j == m)) {
			return i;
		}
	}
	return (-1);
}

int main() {
	return 0;
}
