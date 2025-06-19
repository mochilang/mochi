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

int isMatch(char* s, char* p){
	int m = strlen(s);
	int n = strlen(p);
	list_int _t1 = list_int_create(0);
	list_list_int dp = _t1;
	int i = 0;
	while ((i <= m)) {
		list_int _t2 = list_int_create(0);
		list_int row = _t2;
		int j = 0;
		while ((j <= n)) {
			list_int _t3 = list_int_create(1);
			_t3.data[0] = 0;
			row = (row + _t3);
			j = (j + 1);
		}
		list_int _t4 = list_int_create(1);
		_t4.data[0] = row;
		dp = (dp + _t4);
		i = (i + 1);
	}
	dp.data[m] = 1;
	int i2 = m;
	while ((i2 >= 0)) {
		int j2 = (n - 1);
		while ((j2 >= 0)) {
			int first = 0;
			if ((i2 < m)) {
				char* _t5 = _index_string(p, j2);
				char* _t6 = _index_string(s, i2);
				char* _t7 = _index_string(p, j2);
				if ((((_t5 == _t6)) || ((_t7 == ".")))) {
					first = 1;
				}
			}
			int star = 0;
			if (((j2 + 1) < n)) {
				char* _t8 = _index_string(p, (j2 + 1));
				if ((_t8 == "*")) {
					star = 1;
				}
			}
			if (star) {
				if ((dp.data[i2].data[(j2 + 2)] || ((first && dp.data[(i2 + 1)].data[j2])))) {
					dp.data[i2] = 1;
				} else {
					dp.data[i2] = 0;
				}
			} else {
				if ((first && dp.data[(i2 + 1)].data[(j2 + 1)])) {
					dp.data[i2] = 1;
				} else {
					dp.data[i2] = 0;
				}
			}
			j2 = (j2 - 1);
		}
		i2 = (i2 - 1);
	}
	return dp.data[0].data[0];
}

int main() {
	return 0;
}
