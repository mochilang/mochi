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


static char* concat_string(char* a, char* b) {
	size_t len1 = strlen(a);
	size_t len2 = strlen(b);
	char* buf = (char*)malloc(len1 + len2 + 1);
	memcpy(buf, a, len1);
	memcpy(buf + len1, b, len2);
	buf[len1 + len2] = '\0';
	return buf;
}

char* intToRoman(int num){
	list_int _t1 = list_int_create(13);
	_t1.data[0] = 1000;
	_t1.data[1] = 900;
	_t1.data[2] = 500;
	_t1.data[3] = 400;
	_t1.data[4] = 100;
	_t1.data[5] = 90;
	_t1.data[6] = 50;
	_t1.data[7] = 40;
	_t1.data[8] = 10;
	_t1.data[9] = 9;
	_t1.data[10] = 5;
	_t1.data[11] = 4;
	_t1.data[12] = 1;
	list_int values = _t1;
	list_string _t2 = list_string_create(13);
	_t2.data[0] = "M";
	_t2.data[1] = "CM";
	_t2.data[2] = "D";
	_t2.data[3] = "CD";
	_t2.data[4] = "C";
	_t2.data[5] = "XC";
	_t2.data[6] = "L";
	_t2.data[7] = "XL";
	_t2.data[8] = "X";
	_t2.data[9] = "IX";
	_t2.data[10] = "V";
	_t2.data[11] = "IV";
	_t2.data[12] = "I";
	list_string symbols = _t2;
	char* result = "";
	int i = 0;
	while ((num > 0)) {
		while ((num >= values.data[i])) {
			char* _t3 = concat_string(result, symbols.data[i]);
			result = _t3;
			num = (num - values.data[i]);
		}
		i = (i + 1);
	}
	return result;
}

int main() {
	return 0;
}
