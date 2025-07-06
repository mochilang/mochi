# Errors

- avg_builtin.c: parse error: parse error: 7:13: unexpected token "="
- avg_float_builtin.c: parse error: parse error: 12:13: unexpected token "="
- bool_ops.c: type error: error[T020]: operator `&&` cannot be used on types int and int
  --> :6:10

help:
  Choose an operator that supports these operand types.
- break_continue.c: parse error: parse error: 7:13: unexpected token "="
- count_builtin.c: parse error: parse error: 7:13: unexpected token "="
- dataset_filter_paginate.c: parse error: parse error: 7:13: unexpected token "="
- dataset_skip_take.c: parse error: parse error: 7:13: unexpected token "="
- fetch_builtin.c: line 15:8: unknown type name 'map_string'
  14| }
  15| static map_string _fetch(const char *url, void *opts) {
            ^
  16|   (void)opts;
  17|   char *data = NULL;
line 19:12: call to undeclared function '_read_all'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  18|   if (strncmp(url, "file://", 7) == 0) {
  19|     data = _read_all(url + 7);
                ^
  20|   } else if (strncmp(url, "file:", 5) == 0) {
  21|     data = _read_all(url + 5);
line 19:10: incompatible integer to pointer conversion assigning to 'char *' from 'int' [-Wint-conversion]
  18|   if (strncmp(url, "file://", 7) == 0) {
  19|     data = _read_all(url + 7);
              ^
  20|   } else if (strncmp(url, "file:", 5) == 0) {
  21|     data = _read_all(url + 5);
line 21:12: call to undeclared function '_read_all'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  20|   } else if (strncmp(url, "file:", 5) == 0) {
  21|     data = _read_all(url + 5);
                ^
  22|   } else {
  23|     char cmd[512];
line 21:10: incompatible integer to pointer conversion assigning to 'char *' from 'int' [-Wint-conversion]
  20|   } else if (strncmp(url, "file:", 5) == 0) {
  21|     data = _read_all(url + 5);
              ^
  22|   } else {
  23|     char cmd[512];
line 46:3: use of undeclared identifier 'list_map_string'
  45|     data = strdup("");
  46|   list_map_string rows = _parse_json(data);
       ^
  47|   free(data);
  48|   if (rows.len > 0)
line 48:7: use of undeclared identifier 'rows'
  47|   free(data);
  48|   if (rows.len > 0)
           ^
  49|     return rows.data[0];
  50|   return map_string_create(0);
line 49:12: use of undeclared identifier 'rows'
  48|   if (rows.len > 0)
  49|     return rows.data[0];
                ^
  50|   return map_string_create(0);
  51| }
line 50:10: call to undeclared function 'map_string_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  49|     return rows.data[0];
  50|   return map_string_create(0);
              ^
  51| }
  52| int main() {
line 54:22: member reference base type 'int' is not a structure or union
  53|   int data = (_fetch("file:../../tests/compiler/c/fetch_builtin.json", NULL));
  54|   printf("%s\n", data.data["message"]);
                          ^
  55|   return 0;
  56| }
line 54:27: array subscript is not an integer
  53|   int data = (_fetch("file:../../tests/compiler/c/fetch_builtin.json", NULL));
  54|   printf("%s\n", data.data["message"]);
                               ^
  55|   return 0;
  56| }

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: #include <string.h>
  4: 
  5: typedef struct {
  6:   int len;
  7:   int *data;
  8: } list_int;
  9: static list_int list_int_create(int len) {
 10:   list_int l;
- for_list_collection.c: parse error: parse error: 7:13: unexpected token "="
- for_string_collection.c: parse error: parse error: 6:17: lexer: invalid input text "; \"hi\"[_t1] != '..."
- fun_expr.c: vm run error:
call stack exceeded 1024 frames
call graph: main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main -> main
stack trace:
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main at :9
    print((_lambda0)(2, 3))
  main:0

- fun_expr_in_let.c: parse error: parse error: 9:5: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- group_by.c: line 103:26: use of undeclared identifier 'k'
 102|   map_int_bool _t3 = map_int_bool_create(2);
 103|   map_int_bool_put(&_t3, k, g.key);
                              ^
 104|   map_int_bool_put(&_t3, c, g.items.len);
 105|   list_int _t4 = list_int_create(_t2.len);
line 103:29: use of undeclared identifier 'g'
 102|   map_int_bool _t3 = map_int_bool_create(2);
 103|   map_int_bool_put(&_t3, k, g.key);
                                 ^
 104|   map_int_bool_put(&_t3, c, g.items.len);
 105|   list_int _t4 = list_int_create(_t2.len);
line 104:26: use of undeclared identifier 'c'
 103|   map_int_bool_put(&_t3, k, g.key);
 104|   map_int_bool_put(&_t3, c, g.items.len);
                              ^
 105|   list_int _t4 = list_int_create(_t2.len);
 106|   int _t5 = 0;
line 104:29: use of undeclared identifier 'g'
 103|   map_int_bool_put(&_t3, k, g.key);
 104|   map_int_bool_put(&_t3, c, g.items.len);
                                 ^
 105|   list_int _t4 = list_int_create(_t2.len);
 106|   int _t5 = 0;
line 109:19: assigning to 'int' from incompatible type 'map_int_bool'
 108|     _GroupInt g = _t2.data[i];
 109|     _t4.data[_t5] = _t3;
                       ^
 110|     _t5++;
 111|   }
line 116:20: member reference base type 'int' is not a structure or union
 115|     int g = groups.data[_t6];
 116|     printf("%d ", g.k);
                        ^
 117|     printf("%d\n", g.c);
 118|   }
line 117:21: member reference base type 'int' is not a structure or union
 116|     printf("%d ", g.k);
 117|     printf("%d\n", g.c);
                         ^
 118|   }
 119|   return 0;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- higher_order_apply.c: line 18:24: expected ')'
  17| 
  18| int apply(int (*)(int) f, int x) { return f(x); }
                            ^
  19| 
  20| int main() {
line 18:43: call to undeclared function 'f'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  17| 
  18| int apply(int (*)(int) f, int x) { return f(x); }
                                               ^
  19| 
  20| int main() {
line 18:45: use of undeclared identifier 'x'
  17| 
  18| int apply(int (*)(int) f, int x) { return f(x); }
                                                 ^
  19| 
  20| int main() {
line 21:29: too many arguments to function call, expected 1, have 2
  20| int main() {
  21|   printf("%d\n", apply(inc, 5));
                                 ^
  22|   printf("%d\n", apply(_lambda0, 7));
  23|   return 0;
line 22:34: too many arguments to function call, expected 1, have 2
  21|   printf("%d\n", apply(inc, 5));
  22|   printf("%d\n", apply(_lambda0, 7));
                                      ^
  23|   return 0;
  24| }

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- input_builtin.c: line 17:12: call to undeclared library function 'strdup' with type 'char *(const char *)'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  16|   if (!fgets(buf, sizeof(buf), stdin))
  17|     return strdup("");
                ^
  18|   size_t len = strlen(buf);
  19|   if (len > 0 && buf[len - 1] == '\n')
line 18:16: call to undeclared library function 'strlen' with type 'unsigned long (const char *)'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  17|     return strdup("");
  18|   size_t len = strlen(buf);
                    ^
  19|   if (len > 0 && buf[len - 1] == '\n')
  20|     buf[len - 1] = '\0';

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- json_builtin.c: parse error: parse error: 98:9: lexer: invalid input text "\\n\")\n"
- len_builtin.c: parse error: parse error: 7:13: unexpected token "="
- list_concat.c: parse error: parse error: 14:15: unexpected token "=" (expected "}")
- list_except.c: parse error: parse error: 22:7: unexpected token "r" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- list_float_ops.c: parse error: parse error: 12:7: unexpected token "," (expected "}")
- list_index.c: parse error: parse error: 7:13: unexpected token "="
- list_intersect.c: parse error: parse error: 12:3: unexpected token "." (expected "}")
- list_list_except.c: line 30:11: call to undeclared function 'equal_list_int'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  29|     for (int j = 0; j < b.len; j++)
  30|       if (equal_list_int(a.data[i], b.data[j])) {
               ^
  31|         found = 1;
  32|         break;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- list_list_intersect.c: line 30:11: call to undeclared function 'equal_list_int'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  29|     for (int j = 0; j < b.len; j++)
  30|       if (equal_list_int(a.data[i], b.data[j])) {
               ^
  31|         found = 1;
  32|         break;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- list_list_membership.c: line 26:9: call to undeclared function 'equal_list_int'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  25|   for (int i = 0; i < v.len; i++)
  26|     if (equal_list_int(v.data[i], item))
             ^
  27|       return 1;
  28|   return 0;
line 51:19: passing 'int' to parameter of incompatible type 'list_int'
  50|   _t2.data[1] = _t4;
  51|   _print_list_int(contains_list_list_int(_t2, _t1));
                       ^
  52|   printf("\n");
  53|   return 0;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- list_list_union.c: line 30:11: call to undeclared function 'equal_list_int'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  29|     for (int j = 0; j < idx; j++)
  30|       if (equal_list_int(r.data[j], a.data[i])) {
               ^
  31|         found = 1;
  32|         break;
line 40:11: call to undeclared function 'equal_list_int'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  39|     for (int j = 0; j < idx; j++)
  40|       if (equal_list_int(r.data[j], b.data[i])) {
               ^
  41|         found = 1;
  42|         break;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- list_prepend.c: parse error: parse error: 14:15: unexpected token "=" (expected PostfixExpr)
- list_slice.c: parse error: parse error: 13:3: unexpected token "start" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- list_string_param.c: parse error: parse error: 15:13: unexpected token "="
- list_union.c: parse error: parse error: 13:15: unexpected token "=" (expected "}")
- list_union_all.c: parse error: parse error: 14:15: unexpected token "=" (expected "}")
- load_save_json.c: line 236:3: typedef redefinition with different types ('struct Person' (aka 'Person') vs 'struct Person')
 235|   int age;
 236| } Person;
       ^
 237| 
 238| int main() {
line 239:3: use of undeclared identifier 'list_Person'
 238| int main() {
 239|   list_Person people = _load_json("");
       ^
 240|   _save_json(people, "");
 241|   return 0;
line 240:14: use of undeclared identifier 'people'
 239|   list_Person people = _load_json("");
 240|   _save_json(people, "");
                  ^
 241|   return 0;
 242| }

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: #include <string.h>
  4: 
  5: typedef struct {
  6:   int len;
  7:   int *data;
  8: } list_int;
  9: static list_int list_int_create(int len) {
 10:   list_int l;
- map_membership.c: line 60:3: typedef redefinition with different types ('struct map_int_bool_item' vs 'struct map_int_bool_item')
  59|   int value;
  60| } map_int_bool_item;
       ^
  61| static map_int_bool_item *map_int_bool_item_new(int key, int value) {
  62|   map_int_bool_item *it =
line 61:27: redefinition of 'map_int_bool_item_new'
  60| } map_int_bool_item;
  61| static map_int_bool_item *map_int_bool_item_new(int key, int value) {
                               ^
  62|   map_int_bool_item *it =
  63|       (map_int_bool_item *)malloc(sizeof(map_int_bool_item));
line 72:3: typedef redefinition with different types ('struct map_int_bool' vs 'struct map_int_bool')
  71|   map_int_bool_item **data;
  72| } map_int_bool;
       ^
  73| static map_int_bool map_int_bool_create(int cap) {
  74|   map_int_bool m;
line 73:21: redefinition of 'map_int_bool_create'
  72| } map_int_bool;
  73| static map_int_bool map_int_bool_create(int cap) {
                         ^
  74|   map_int_bool m;
  75|   m.len = 0;
line 81:13: redefinition of 'map_int_bool_put'
  80| }
  81| static void map_int_bool_put(map_int_bool *m, int key, int value) {
                 ^
  82|   for (int i = 0; i < m->len; i++)
  83|     if (m->data[i]->key == key) {
line 94:12: redefinition of 'map_int_bool_contains'
  93| }
  94| static int map_int_bool_contains(map_int_bool m, int key) {
                ^
  95|   for (int i = 0; i < m.len; i++)
  96|     if (m.data[i]->key == key)

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- match_basic.c: parse error: parse error: 7:18: lexer: invalid input text "? \"one\" : (x == ..."
- matrix_search.c: parse error: parse error: 13:15: unexpected token "=" (expected "}")
- max_builtin.c: parse error: parse error: 11:17: lexer: invalid input text "? _t1.data[0] : ..."
- membership.c: parse error: parse error: 14:5: unexpected token "return" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- min_builtin.c: parse error: parse error: 11:17: lexer: invalid input text "? _t1.data[0] : ..."
- now_builtin.c: parse error: parse error: 8:33: lexer: invalid input text "&ts)\n  return (l..."
- slice_remove.c: parse error: parse error: 12:4: unexpected token ")" (expected "}")
- str_builtin.c: parse error: parse error: 7:7: unexpected token "*" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- string_cmp.c: type error: error[T003]: unknown function: strcmp
  --> :6:8

help:
  Ensure the function is defined before it's called.
- string_concat.c: parse error: parse error: 12:22: lexer: invalid input text "'\\0'\n  return bu..."
- string_for_loop.c: parse error: parse error: 6:17: lexer: invalid input text "; \"cat\"[_t1] != ..."
- string_in.c: type error: error[T003]: unknown function: strstr
  --> :7:10

help:
  Ensure the function is defined before it's called.
- string_index.c: parse error: parse error: 18:9: lexer: invalid input text "'\\0'\n_b\n})\nprint..."
- string_len.c: type error: error[T003]: unknown function: strlen
  --> :6:7

help:
  Ensure the function is defined before it's called.
- string_negative_index.c: parse error: parse error: 18:9: lexer: invalid input text "'\\0'\n_b\n})\nprint..."
- string_slice.c: parse error: parse error: 22:15: lexer: invalid input text "'\\0'\n_b\n})\nprint..."
- struct_basic.c: line 19:3: typedef redefinition with different types ('struct Pair' (aka 'Pair') vs 'struct Pair')
  18|   int y;
  19| } Pair;
       ^
  20| 
  21| int main() {
line 22:12: variable has incomplete type 'Pair' (aka 'struct Pair')
  21| int main() {
  22|   Pair p = (Pair){.x = 1, .y = 2};
                ^
  23|   printf("%d\n", p.x);
  24|   printf("%d\n", p.y);
line 22:8: variable has incomplete type 'Pair' (aka 'struct Pair')
  21| int main() {
  22|   Pair p = (Pair){.x = 1, .y = 2};
            ^
  23|   printf("%d\n", p.x);
  24|   printf("%d\n", p.y);

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- sum_builtin.c: parse error: parse error: 7:13: unexpected token "="
- sum_float_builtin.c: parse error: parse error: 12:13: unexpected token "="
- two_sum.c: parse error: parse error: 12:21: unexpected token "=" (expected "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- type_method.c: line 28:3: typedef redefinition with different types ('struct Person' (aka 'Person') vs 'struct Person')
  27|   char *name;
  28| } Person;
       ^
  29| char *Person_greet(Person *self) {
  30|   char *_t1 = concat_string("hi ", self->name);
line 30:40: incomplete definition of type 'Person' (aka 'struct Person')
  29| char *Person_greet(Person *self) {
  30|   char *_t1 = concat_string("hi ", self->name);
                                            ^
  31|   return _t1;
  32| }
line 35:14: variable has incomplete type 'Person' (aka 'struct Person')
  34| int main() {
  35|   Person p = (Person){.name = "Ada"};
                  ^
  36|   printf("%s\n", Person_greet(&p));
  37|   return 0;
line 35:10: variable has incomplete type 'Person' (aka 'struct Person')
  34| int main() {
  35|   Person p = (Person){.name = "Ada"};
              ^
  36|   printf("%s\n", Person_greet(&p));
  37|   return 0;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: #include <string.h>
  4: 
  5: typedef struct {
  6:   int len;
  7:   int *data;
  8: } list_int;
  9: static list_int list_int_create(int len) {
 10:   list_int l;
- union_field.c: line 19:3: typedef redefinition with different types ('struct Leaf' (aka 'Leaf') vs 'struct Leaf')
  18| typedef struct {
  19| } Leaf;
       ^
  20| typedef struct {
  21|   Tree left;
line 21:8: field has incomplete type 'Tree' (aka 'struct Tree')
  20| typedef struct {
  21|   Tree left;
            ^
  22|   int value;
  23|   Tree right;
line 23:8: field has incomplete type 'Tree' (aka 'struct Tree')
  22|   int value;
  23|   Tree right;
            ^
  24| } Node;
  25| typedef struct {
line 24:3: typedef redefinition with different types ('struct (unnamed struct at <stdin>:20:9)' vs 'struct Node')
  23|   Tree right;
  24| } Node;
       ^
  25| typedef struct {
  26|   int tag;
line 28:10: field has incomplete type 'Leaf' (aka 'struct Leaf')
  27|   union {
  28|     Leaf Leaf;
              ^
  29|     Node Node;
  30|   } value;
line 29:10: field has incomplete type 'Node' (aka 'struct Node')
  28|     Leaf Leaf;
  29|     Node Node;
              ^
  30|   } value;
  31| } Tree;
line 31:3: typedef redefinition with different types ('struct (unnamed struct at <stdin>:25:9)' vs 'struct Tree')
  30|   } value;
  31| } Tree;
       ^
  32| 
  33| int main() {
line 34:27: variable has incomplete type 'Leaf' (aka 'struct Leaf')
  33| int main() {
  34|   Node t = (Node){.left = (Leaf){}, .value = 42, .right = (Leaf){}};
                               ^
  35|   printf("%d\n", t.value);
  36|   return 0;
line 34:59: variable has incomplete type 'Leaf' (aka 'struct Leaf')
  33| int main() {
  34|   Node t = (Node){.left = (Leaf){}, .value = 42, .right = (Leaf){}};
                                                               ^
  35|   printf("%d\n", t.value);
  36|   return 0;
line 34:8: variable has incomplete type 'Node' (aka 'struct Node')
  33| int main() {
  34|   Node t = (Node){.left = (Leaf){}, .value = 42, .right = (Leaf){}};
            ^
  35|   printf("%d\n", t.value);
  36|   return 0;

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
- update_statement.c: line 20:3: typedef redefinition with different types ('struct Person' (aka 'Person') vs 'struct Person')
  19|   char *status;
  20| } Person;
       ^
  21| 
  22| static void test_update_adult_status() {
line 24:17: variable has incomplete type 'Person' (aka 'struct Person')
  23|   list_int _t1 = list_int_create(4);
  24|   _t1.data[0] = (Person){.name = "Alice", .age = 17, .status = "minor"};
                     ^
  25|   _t1.data[1] = (Person){.name = "Bob", .age = 26, .status = "adult"};
  26|   _t1.data[2] = (Person){.name = "Charlie", .age = 19, .status = "adult"};
line 25:17: variable has incomplete type 'Person' (aka 'struct Person')
  24|   _t1.data[0] = (Person){.name = "Alice", .age = 17, .status = "minor"};
  25|   _t1.data[1] = (Person){.name = "Bob", .age = 26, .status = "adult"};
                     ^
  26|   _t1.data[2] = (Person){.name = "Charlie", .age = 19, .status = "adult"};
  27|   _t1.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
line 26:17: variable has incomplete type 'Person' (aka 'struct Person')
  25|   _t1.data[1] = (Person){.name = "Bob", .age = 26, .status = "adult"};
  26|   _t1.data[2] = (Person){.name = "Charlie", .age = 19, .status = "adult"};
                     ^
  27|   _t1.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
  28|   if (!((people == _t1))) {
line 27:17: variable has incomplete type 'Person' (aka 'struct Person')
  26|   _t1.data[2] = (Person){.name = "Charlie", .age = 19, .status = "adult"};
  27|   _t1.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
                     ^
  28|   if (!((people == _t1))) {
  29|     fprintf(stderr, "expect failed\n");
line 28:10: use of undeclared identifier 'people'
  27|   _t1.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
  28|   if (!((people == _t1))) {
              ^
  29|     fprintf(stderr, "expect failed\n");
  30|     exit(1);
line 36:17: variable has incomplete type 'Person' (aka 'struct Person')
  35|   list_int _t2 = list_int_create(4);
  36|   _t2.data[0] = (Person){.name = "Alice", .age = 17, .status = "minor"};
                     ^
  37|   _t2.data[1] = (Person){.name = "Bob", .age = 25, .status = "unknown"};
  38|   _t2.data[2] = (Person){.name = "Charlie", .age = 18, .status = "unknown"};
line 37:17: variable has incomplete type 'Person' (aka 'struct Person')
  36|   _t2.data[0] = (Person){.name = "Alice", .age = 17, .status = "minor"};
  37|   _t2.data[1] = (Person){.name = "Bob", .age = 25, .status = "unknown"};
                     ^
  38|   _t2.data[2] = (Person){.name = "Charlie", .age = 18, .status = "unknown"};
  39|   _t2.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
line 38:17: variable has incomplete type 'Person' (aka 'struct Person')
  37|   _t2.data[1] = (Person){.name = "Bob", .age = 25, .status = "unknown"};
  38|   _t2.data[2] = (Person){.name = "Charlie", .age = 18, .status = "unknown"};
                     ^
  39|   _t2.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
  40|   list_Person people = _t2;
line 39:17: variable has incomplete type 'Person' (aka 'struct Person')
  38|   _t2.data[2] = (Person){.name = "Charlie", .age = 18, .status = "unknown"};
  39|   _t2.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
                     ^
  40|   list_Person people = _t2;
  41|   for (int _t3 = 0; _t3 < people.len; _t3++) {
line 40:3: use of undeclared identifier 'list_Person'
  39|   _t2.data[3] = (Person){.name = "Diana", .age = 16, .status = "minor"};
  40|   list_Person people = _t2;
       ^
  41|   for (int _t3 = 0; _t3 < people.len; _t3++) {
  42|     Person _t4 = people.data[_t3];
line 41:27: use of undeclared identifier 'people'
  40|   list_Person people = _t2;
  41|   for (int _t3 = 0; _t3 < people.len; _t3++) {
                               ^
  42|     Person _t4 = people.data[_t3];
  43|     char *name = _t4.name;
line 42:18: use of undeclared identifier 'people'
  41|   for (int _t3 = 0; _t3 < people.len; _t3++) {
  42|     Person _t4 = people.data[_t3];
                      ^
  43|     char *name = _t4.name;
  44|     int age = _t4.age;
line 42:12: variable has incomplete type 'Person' (aka 'struct Person')
  41|   for (int _t3 = 0; _t3 < people.len; _t3++) {
  42|     Person _t4 = people.data[_t3];
                ^
  43|     char *name = _t4.name;
  44|     int age = _t4.age;
line 46:11: redefinition of 'name'
  45|     char *status = _t4.status;
  46|     char *name = _t4.name;
               ^
  47|     int age = _t4.age;
  48|     char *status = _t4.status;
line 47:9: redefinition of 'age'
  46|     char *name = _t4.name;
  47|     int age = _t4.age;
             ^
  48|     char *status = _t4.status;
  49|     if ((_t4.age >= 18)) {
line 48:11: redefinition of 'status'
  47|     int age = _t4.age;
  48|     char *status = _t4.status;
               ^
  49|     if ((_t4.age >= 18)) {
  50|       _t4.status = "adult";
line 53:5: use of undeclared identifier 'people'
  52|     }
  53|     people.data[_t3] = _t4;
         ^
  54|   }
  55|   test_update_adult_status();

source snippet:
  1: #include <stdio.h>
  2: #include <stdlib.h>
  3: 
  4: typedef struct {
  5:   int len;
  6:   int *data;
  7: } list_int;
  8: static list_int list_int_create(int len) {
  9:   list_int l;
 10:   l.len = len;
