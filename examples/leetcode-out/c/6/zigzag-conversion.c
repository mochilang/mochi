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

char* convert(char* s, int numRows){
	if ((((numRows <= 1) || numRows) >= strlen(s))) {
		return s;
	}
	list_int _t1 = list_int_create(0);
	list_int rows = _t1;
	int i = 0;
	while ((i < numRows)) {
		list_int _t2 = list_int_create(1);
		_t2.data[0] = "";
		rows = (rows + _t2);
		i = (i + 1);
	}
	int curr = 0;
	int step = 1;
	for (int _t3 = 0; s[_t3] != '\0'; _t3++) {
		char ch[2];
		ch[0] = s[_t3];
		ch[1] = '\0';
		rows.data[curr] = (rows.data[curr] + ch);
		if ((curr == 0)) {
			step = 1;
		} else 		if (((curr == numRows) - 1)) {
			step = (-1);
		}
		curr = (curr + step);
	}
	char* result = "";
	for (int _t4 = 0; _t4 < rows.len; _t4++) {
		int row = rows.data[_t4];
		result = (result + row);
	}
	return result;
}

int main() {
	return 0;
}
