#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { int len; int *data; } list_int;
typedef struct { int len; char** data; } list_string;

static list_int list_int_create(int len) {
	list_int l;
	l.len = len;
	l.data = (int*)malloc(sizeof(int)*len);
	return l;
}

static list_string list_string_create(int len) {
	list_string l;
	l.len = len;
	l.data = (char**)malloc(sizeof(char*)*len);
	return l;
}

static list_string concat_list_string(list_string a, list_string b) {
	list_string r = list_string_create(a.len + b.len);
	for (int i = 0; i < a.len; i++) {
		r.data[i] = a.data[i];
	}
	for (int i = 0; i < b.len; i++) {
		r.data[a.len + i] = b.data[i];
	}
	return r;
}


static char* concat_string(char* a, char* b) {
	size_t len1 = strlen(a);
	size_t len2 = strlen(b);
	char* buf = (char*)malloc(len1 + len2 + 1);
	memcpy(buf, a, len1);
	memcpy(buf + len1, b, len2);
	buf[len1 + len2] = '\0';
	return buf;
}

char* convert(char* s, int numRows){
	if ((((numRows <= 1) || numRows) >= strlen(s))) {
		return s;
	}
	list_string _t1 = list_string_create(0);
	list_string rows = _t1;
	int i = 0;
	while ((i < numRows)) {
		list_string _t2 = list_string_create(1);
		_t2.data[0] = "";
		list_string _t3 = concat_list_string(rows, _t2);
		rows = _t3;
		i = (i + 1);
	}
	int curr = 0;
	int step = 1;
	for (int _t4 = 0; s[_t4] != '\0'; _t4++) {
		char ch[2];
		ch[0] = s[_t4];
		ch[1] = '\0';
		char* _t5 = concat_string(rows.data[curr], ch);
		rows.data[curr] = _t5;
		if ((curr == 0)) {
			step = 1;
		} else 		if (((curr == numRows) - 1)) {
			step = (-1);
		}
		curr = (curr + step);
	}
	char* result = "";
	for (int _t6 = 0; _t6 < rows.len; _t6++) {
		char* row = rows.data[_t6];
		char* _t7 = concat_string(result, row);
		result = _t7;
	}
	return result;
}

int main() {
	return 0;
}
