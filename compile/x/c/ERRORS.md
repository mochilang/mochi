# Errors

- append_builtin: convert error: exit status 1
line 19:18: call to undeclared function 'append'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  18|   list_int a = _t1;
  19|   printf("%d\n", append(a, 3));
                      ^
  20|   return 0;
  21| }

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
 10:   l.len = len;exit status 1

- avg_builtin: parse roundtrip error: parse error: 12:15: lexer: invalid input text "; i < _t1.len; i..."
- basic_compare: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- binary_precedence: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- bool_chain: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- break_continue: parse roundtrip error: parse error: 17:17: lexer: invalid input text "; _t2 < numbers...."
- cast_string_to_int: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- cast_struct: convert error: exit status 1
line 61:3: typedef redefinition with different types ('struct Todo' (aka 'Todo') vs 'struct Todo')
  60|   char *title;
  61| } Todo;
       ^
  62| 
  63| int main() {
line 65:26: incompatible pointer to integer conversion passing 'char[6]' to parameter of type 'int' [-Wint-conversion]
  64|   map_int_bool _t1 = map_int_bool_create(1);
  65|   map_int_bool_put(&_t1, "title", "hi");
                              ^
  66|   Todo todo = _t1;
  67|   printf("%s\n", todo.title);
line 65:35: incompatible pointer to integer conversion passing 'char[3]' to parameter of type 'int' [-Wint-conversion]
  64|   map_int_bool _t1 = map_int_bool_create(1);
  65|   map_int_bool_put(&_t1, "title", "hi");
                                       ^
  66|   Todo todo = _t1;
  67|   printf("%s\n", todo.title);
line 66:8: variable has incomplete type 'Todo' (aka 'struct Todo')
  65|   map_int_bool_put(&_t1, "title", "hi");
  66|   Todo todo = _t1;
            ^
  67|   printf("%s\n", todo.title);
  68|   return 0;

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
 10:   l.len = len;exit status 1

- closure: convert error: exit status 1
line 14:35: use of undeclared identifier 'n'
  13| }
  14| int _lambda0(int x) { return (x + n); }
                                       ^
  15| 
  16| int (*)(int) makeAdder(int n) { return _lambda0; }
line 16:7: expected identifier or '('
  15| 
  16| int (*)(int) makeAdder(int n) { return _lambda0; }
           ^
  17| 
  18| int main() {
line 19:23: call to undeclared function 'makeAdder'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  18| int main() {
  19|   int (*add10)(int) = makeAdder(10);
                           ^
  20|   printf("%d\n", add10(7));
  21|   return 0;
line 19:9: incompatible integer to pointer conversion initializing 'int (*)(int)' with an expression of type 'int' [-Wint-conversion]
  18| int main() {
  19|   int (*add10)(int) = makeAdder(10);
             ^
  20|   printf("%d\n", add10(7));
  21|   return 0;

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
 10:   l.len = len;exit status 1

- count_builtin: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- cross_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(3);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(3);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 43:58: function definition is not allowed here
  42|   } list_ordersItem;
  43|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  44|     list_ordersItem l;
  45|     l.len = len;
line 49:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  48|   }
  49|   list_ordersItem _t2 = list_ordersItem_create(3);
                             ^
  50|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  51|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 49:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  48|   }
  49|   list_ordersItem _t2 = list_ordersItem_create(3);
                       ^
  50|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  51|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 54:12: initializing 'list_int' with an expression of incompatible type 'int'
  53|   list_ordersItem orders = _t2;
  54|   list_int result = 0;
                ^
  55|   printf("%s\n", "--- Cross Join: All order-customer pairs ---");
  56|   for (int _t3 = 0; _t3 < result.len; _t3++) {
line 59:24: member reference base type 'int' is not a structure or union
  58|     printf("%s ", "Order");
  59|     printf("%d ", entry.orderId);
                            ^
  60|     printf("%s ", "(customerId:");
  61|     printf("%d ", entry.orderCustomerId);
line 61:24: member reference base type 'int' is not a structure or union
  60|     printf("%s ", "(customerId:");
  61|     printf("%d ", entry.orderCustomerId);
                            ^
  62|     printf("%s ", ", total: $");
  63|     printf("%d ", entry.orderTotal);
line 63:24: member reference base type 'int' is not a structure or union
  62|     printf("%s ", ", total: $");
  63|     printf("%d ", entry.orderTotal);
                            ^
  64|     printf("%s ", ") paired with");
  65|     printf("%d\n", entry.pairedCustomerName);
line 65:25: member reference base type 'int' is not a structure or union
  64|     printf("%s ", ") paired with");
  65|     printf("%d\n", entry.pairedCustomerName);
                             ^
  66|   }
  67|   return 0;

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
 10:   l.len = len;exit status 1

- cross_join_filter: convert error: exit status 1
line 35:12: initializing 'list_int' with an expression of incompatible type 'int'
  34|   list_string letters = _t2;
  35|   list_int pairs = 0;
                ^
  36|   printf("%s\n", "--- Even pairs ---");
  37|   for (int _t3 = 0; _t3 < pairs.len; _t3++) {
line 39:20: member reference base type 'int' is not a structure or union
  38|     int p = pairs.data[_t3];
  39|     printf("%d ", p.n);
                        ^
  40|     printf("%d\n", p.l);
  41|   }
line 40:21: member reference base type 'int' is not a structure or union
  39|     printf("%d ", p.n);
  40|     printf("%d\n", p.l);
                         ^
  41|   }
  42|   return 0;

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
 10:   list_int l;exit status 1

- cross_join_triple: convert error: exit status 1
line 38:12: initializing 'list_int' with an expression of incompatible type 'int'
  37|   list_int bools = _t3;
  38|   list_int combos = 0;
                ^
  39|   printf("%s\n", "--- Cross Join of three lists ---");
  40|   for (int _t4 = 0; _t4 < combos.len; _t4++) {
line 42:20: member reference base type 'int' is not a structure or union
  41|     int c = combos.data[_t4];
  42|     printf("%d ", c.n);
                        ^
  43|     printf("%d ", c.l);
  44|     printf("%d\n", c.b);
line 43:20: member reference base type 'int' is not a structure or union
  42|     printf("%d ", c.n);
  43|     printf("%d ", c.l);
                        ^
  44|     printf("%d\n", c.b);
  45|   }
line 44:21: member reference base type 'int' is not a structure or union
  43|     printf("%d ", c.l);
  44|     printf("%d\n", c.b);
                         ^
  45|   }
  46|   return 0;

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
 10:   list_int l;exit status 1

- dataset_sort_take_limit: convert error: exit status 1
line 23:62: function definition is not allowed here
  22|   } list_productsItem;
  23|   static list_productsItem list_productsItem_create(int len) {
                                                                  ^
  24|     list_productsItem l;
  25|     l.len = len;
line 29:27: call to undeclared function 'list_productsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_productsItem _t1 = list_productsItem_create(7);
                               ^
  30|   _t1.data[0] = (productsItem){.name = "Laptop", .price = 1500};
  31|   _t1.data[1] = (productsItem){.name = "Smartphone", .price = 900};
line 29:21: initializing 'list_productsItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_productsItem _t1 = list_productsItem_create(7);
                         ^
  30|   _t1.data[0] = (productsItem){.name = "Laptop", .price = 1500};
  31|   _t1.data[1] = (productsItem){.name = "Smartphone", .price = 900};
line 38:21: initializing 'list_productsItem' with an expression of incompatible type 'int'
  37|   list_productsItem products = _t1;
  38|   list_productsItem _t2 = list_productsItem_create(products.len);
                         ^
  39|   int *_t5 = (int *)malloc(sizeof(int) * products.len);
  40|   int _t3 = 0;
line 73:11: initializing 'int' with an expression of incompatible type 'productsItem'
  72|     for (int _t11 = 0; _t11 < expensive.len; _t11++) {
  73|       int item = expensive.data[_t11];
               ^
  74|       printf("%d ", item.name);
  75|       printf("%s ", "costs $");
line 74:25: member reference base type 'int' is not a structure or union
  73|       int item = expensive.data[_t11];
  74|       printf("%d ", item.name);
                             ^
  75|       printf("%s ", "costs $");
  76|       printf("%d\n", item.price);
line 76:26: member reference base type 'int' is not a structure or union
  75|       printf("%s ", "costs $");
  76|       printf("%d\n", item.price);
                              ^
  77|     }
  78|     return 0;
line 79:4: expected '}'
  78|     return 0;
  79|   }
        ^
  80|

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
 10:   l.len = len;exit status 1

- dataset_where_filter: convert error: exit status 1
line 66:58: function definition is not allowed here
  65|   } list_peopleItem;
  66|   static list_peopleItem list_peopleItem_create(int len) {
                                                              ^
  67|     list_peopleItem l;
  68|     l.len = len;
line 72:25: call to undeclared function 'list_peopleItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  71|   }
  72|   list_peopleItem _t1 = list_peopleItem_create(4);
                             ^
  73|   _t1.data[0] = (peopleItem){.name = "Alice", .age = 30};
  74|   _t1.data[1] = (peopleItem){.name = "Bob", .age = 15};
line 72:19: initializing 'list_peopleItem' with an expression of incompatible type 'int'
  71|   }
  72|   list_peopleItem _t1 = list_peopleItem_create(4);
                       ^
  73|   _t1.data[0] = (peopleItem){.name = "Alice", .age = 30};
  74|   _t1.data[1] = (peopleItem){.name = "Bob", .age = 15};
line 79:26: use of undeclared identifier 'name'
  78|   map_int_bool _t2 = map_int_bool_create(3);
  79|   map_int_bool_put(&_t2, name, person.name);
                              ^
  80|   map_int_bool_put(&_t2, age, person.age);
  81|   map_int_bool_put(&_t2, is_senior, (person.age >= 60));
line 79:32: use of undeclared identifier 'person'
  78|   map_int_bool _t2 = map_int_bool_create(3);
  79|   map_int_bool_put(&_t2, name, person.name);
                                    ^
  80|   map_int_bool_put(&_t2, age, person.age);
  81|   map_int_bool_put(&_t2, is_senior, (person.age >= 60));
line 80:26: use of undeclared identifier 'age'
  79|   map_int_bool_put(&_t2, name, person.name);
  80|   map_int_bool_put(&_t2, age, person.age);
                              ^
  81|   map_int_bool_put(&_t2, is_senior, (person.age >= 60));
  82|   list_int _t3 = list_int_create(people.len);
line 80:31: use of undeclared identifier 'person'
  79|   map_int_bool_put(&_t2, name, person.name);
  80|   map_int_bool_put(&_t2, age, person.age);
                                   ^
  81|   map_int_bool_put(&_t2, is_senior, (person.age >= 60));
  82|   list_int _t3 = list_int_create(people.len);
line 81:38: use of undeclared identifier 'person'
  80|   map_int_bool_put(&_t2, age, person.age);
  81|   map_int_bool_put(&_t2, is_senior, (person.age >= 60));
                                          ^
  82|   list_int _t3 = list_int_create(people.len);
  83|   int _t4 = 0;
line 81:26: use of undeclared identifier 'is_senior'
  80|   map_int_bool_put(&_t2, age, person.age);
  81|   map_int_bool_put(&_t2, is_senior, (person.age >= 60));
                              ^
  82|   list_int _t3 = list_int_create(people.len);
  83|   int _t4 = 0;
line 89:19: assigning to 'int' from incompatible type 'map_int_bool'
  88|     }
  89|     _t3.data[_t4] = _t2;
                       ^
  90|     _t4++;
  91|   }
line 97:25: member reference base type 'int' is not a structure or union
  96|     int person = adults.data[_t6];
  97|     printf("%d ", person.name);
                             ^
  98|     printf("%s ", "is");
  99|     printf("%d ", person.age);
line 99:25: member reference base type 'int' is not a structure or union
  98|     printf("%s ", "is");
  99|     printf("%d ", person.age);
                             ^
 100|     printf("%d\n", (person.is_senior ? " (senior)" : ""));
 101|   }
line 100:27: member reference base type 'int' is not a structure or union
  99|     printf("%d ", person.age);
 100|     printf("%d\n", (person.is_senior ? " (senior)" : ""));
                               ^
 101|   }
 102|   return 0;

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
 10:   l.len = len;exit status 1

- exists_builtin: convert error: exit status 1
line 30:14: call to undeclared function 'exists'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  29|   _t2.len = _t3;
  30|   int flag = exists(_t2);
                  ^
  31|   printf("%d\n", flag);
  32|   return 0;

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
 10:   l.len = len;exit status 1

- for_list_collection: parse roundtrip error: parse error: 10:17: lexer: invalid input text "; _t2 < _t1.len;..."
- for_loop: parse roundtrip error: parse error: 6:15: lexer: invalid input text "; i < 4; i++) {\n..."
- for_map_collection: convert error: exit status 1
line 59:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(2);
  59|   map_int_bool_put(&_t1, "a", 1);
                              ^
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   int m = _t1;
line 60:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, "a", 1);
  60|   map_int_bool_put(&_t1, "b", 2);
                              ^
  61|   int m = _t1;
  62|   for (int _t2 = 0; _t2 < m.len; _t2++) {
line 61:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   int m = _t1;
           ^
  62|   for (int _t2 = 0; _t2 < m.len; _t2++) {
  63|     int k = m.data[_t2];
line 62:28: member reference base type 'int' is not a structure or union
  61|   int m = _t1;
  62|   for (int _t2 = 0; _t2 < m.len; _t2++) {
                                ^
  63|     int k = m.data[_t2];
  64|     printf("%d\n", k);
line 63:14: member reference base type 'int' is not a structure or union
  62|   for (int _t2 = 0; _t2 < m.len; _t2++) {
  63|     int k = m.data[_t2];
                  ^
  64|     printf("%d\n", k);
  65|   }

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
 10:   l.len = len;exit status 1

- fun_call: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- fun_expr_in_let: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- fun_three_args: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- group_by: convert error: exit status 1
line 24:58: function definition is not allowed here
  23|   } list_peopleItem;
  24|   static list_peopleItem list_peopleItem_create(int len) {
                                                              ^
  25|     list_peopleItem l;
  26|     l.len = len;
line 30:25: call to undeclared function 'list_peopleItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  29|   }
  30|   list_peopleItem _t1 = list_peopleItem_create(6);
                             ^
  31|   _t1.data[0] = (peopleItem){.name = "Alice", .age = 30, .city = "Paris"};
  32|   _t1.data[1] = (peopleItem){.name = "Bob", .age = 15, .city = "Hanoi"};
line 30:19: initializing 'list_peopleItem' with an expression of incompatible type 'int'
  29|   }
  30|   list_peopleItem _t1 = list_peopleItem_create(6);
                       ^
  31|   _t1.data[0] = (peopleItem){.name = "Alice", .age = 30, .city = "Paris"};
  32|   _t1.data[1] = (peopleItem){.name = "Bob", .age = 15, .city = "Hanoi"};
line 38:12: initializing 'list_int' with an expression of incompatible type 'int'
  37|   list_peopleItem people = _t1;
  38|   list_int stats = 0;
                ^
  39|   printf("%s\n", "--- People grouped by city ---");
  40|   for (int _t2 = 0; _t2 < stats.len; _t2++) {
line 42:20: member reference base type 'int' is not a structure or union
  41|     int s = stats.data[_t2];
  42|     printf("%d ", s.city);
                        ^
  43|     printf("%s ", ": count =");
  44|     printf("%d ", s.count);
line 44:20: member reference base type 'int' is not a structure or union
  43|     printf("%s ", ": count =");
  44|     printf("%d ", s.count);
                        ^
  45|     printf("%s ", ", avg_age =");
  46|     printf("%d\n", s.avg_age);
line 46:21: member reference base type 'int' is not a structure or union
  45|     printf("%s ", ", avg_age =");
  46|     printf("%d\n", s.avg_age);
                         ^
  47|   }
  48|   return 0;

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
 10:   l.len = len;exit status 1

- group_by_conditional_sum: convert error: exit status 1
line 24:56: function definition is not allowed here
  23|   } list_itemsItem;
  24|   static list_itemsItem list_itemsItem_create(int len) {
                                                            ^
  25|     list_itemsItem l;
  26|     l.len = len;
line 30:24: call to undeclared function 'list_itemsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  29|   }
  30|   list_itemsItem _t1 = list_itemsItem_create(3);
                            ^
  31|   _t1.data[0] = (itemsItem){.cat = "a", .val = 10, .flag = 1};
  32|   _t1.data[1] = (itemsItem){.cat = "a", .val = 5, .flag = 0};
line 30:18: initializing 'list_itemsItem' with an expression of incompatible type 'int'
  29|   }
  30|   list_itemsItem _t1 = list_itemsItem_create(3);
                      ^
  31|   _t1.data[0] = (itemsItem){.cat = "a", .val = 10, .flag = 1};
  32|   _t1.data[1] = (itemsItem){.cat = "a", .val = 5, .flag = 0};
line 35:12: initializing 'list_int' with an expression of incompatible type 'int'
  34|   list_itemsItem items = _t1;
  35|   list_int result = 0;
                ^
  36|   printf("%d\n", result);
  37|   return 0;

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
 10:   l.len = len;exit status 1

- group_by_having: convert error: exit status 1
line 93:58: function definition is not allowed here
  92|   } list_peopleItem;
  93|   static list_peopleItem list_peopleItem_create(int len) {
                                                              ^
  94|     list_peopleItem l;
  95|     l.len = len;
line 99:25: call to undeclared function 'list_peopleItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  98|   }
  99|   list_peopleItem _t1 = list_peopleItem_create(7);
                             ^
 100|   _t1.data[0] = (peopleItem){.name = "Alice", .city = "Paris"};
 101|   _t1.data[1] = (peopleItem){.name = "Bob", .city = "Hanoi"};
line 99:19: initializing 'list_peopleItem' with an expression of incompatible type 'int'
  98|   }
  99|   list_peopleItem _t1 = list_peopleItem_create(7);
                       ^
 100|   _t1.data[0] = (peopleItem){.name = "Alice", .city = "Paris"};
 101|   _t1.data[1] = (peopleItem){.name = "Bob", .city = "Hanoi"};
line 108:12: initializing 'list_int' with an expression of incompatible type 'int'
 107|   list_peopleItem people = _t1;
 108|   list_int big = 0;
                ^
 109|   _json_int(big);
 110|   return 0;
line 109:13: passing 'list_int' to parameter of incompatible type 'int'
 108|   list_int big = 0;
 109|   _json_int(big);
                 ^
 110|   return 0;
 111| }

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
 10:   list_int l;exit status 1

- group_by_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 41:58: function definition is not allowed here
  40|   } list_ordersItem;
  41|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  42|     list_ordersItem l;
  43|     l.len = len;
line 47:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  46|   }
  47|   list_ordersItem _t2 = list_ordersItem_create(3);
                             ^
  48|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  49|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 1};
line 47:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  46|   }
  47|   list_ordersItem _t2 = list_ordersItem_create(3);
                       ^
  48|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  49|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 1};
line 52:12: initializing 'list_int' with an expression of incompatible type 'int'
  51|   list_ordersItem orders = _t2;
  52|   list_int stats = 0;
                ^
  53|   printf("%s\n", "--- Orders per customer ---");
  54|   for (int _t3 = 0; _t3 < stats.len; _t3++) {
line 56:20: member reference base type 'int' is not a structure or union
  55|     int s = stats.data[_t3];
  56|     printf("%d ", s.name);
                        ^
  57|     printf("%s ", "orders:");
  58|     printf("%d\n", s.count);
line 58:21: member reference base type 'int' is not a structure or union
  57|     printf("%s ", "orders:");
  58|     printf("%d\n", s.count);
                         ^
  59|   }
  60|   return 0;

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
 10:   l.len = len;exit status 1

- group_by_left_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(3);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(3);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 42:58: function definition is not allowed here
  41|   } list_ordersItem;
  42|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  43|     list_ordersItem l;
  44|     l.len = len;
line 48:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  47|   }
  48|   list_ordersItem _t2 = list_ordersItem_create(3);
                             ^
  49|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  50|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 1};
line 48:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  47|   }
  48|   list_ordersItem _t2 = list_ordersItem_create(3);
                       ^
  49|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  50|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 1};
line 55:32: member reference base type 'int' is not a structure or union
  54|   printf("%s\n", "--- Group Left Join ---");
  55|   for (int _t3 = 0; _t3 < stats.len; _t3++) {
                                    ^
  56|     int s = stats.data[_t3];
  57|     printf("%d ", s.name);
line 56:18: member reference base type 'int' is not a structure or union
  55|   for (int _t3 = 0; _t3 < stats.len; _t3++) {
  56|     int s = stats.data[_t3];
                      ^
  57|     printf("%d ", s.name);
  58|     printf("%s ", "orders:");
line 57:20: member reference base type 'int' is not a structure or union
  56|     int s = stats.data[_t3];
  57|     printf("%d ", s.name);
                        ^
  58|     printf("%s ", "orders:");
  59|     printf("%d\n", s.count);
line 59:21: member reference base type 'int' is not a structure or union
  58|     printf("%s ", "orders:");
  59|     printf("%d\n", s.count);
                         ^
  60|   }
  61|   return 0;

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
 10:   l.len = len;exit status 1

- group_by_multi_join: convert error: exit status 1
line 23:60: function definition is not allowed here
  22|   } list_nationsItem;
  23|   static list_nationsItem list_nationsItem_create(int len) {
                                                                ^
  24|     list_nationsItem l;
  25|     l.len = len;
line 29:26: call to undeclared function 'list_nationsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_nationsItem _t1 = list_nationsItem_create(2);
                              ^
  30|   _t1.data[0] = (nationsItem){.id = 1, .name = "A"};
  31|   _t1.data[1] = (nationsItem){.id = 2, .name = "B"};
line 29:20: initializing 'list_nationsItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_nationsItem _t1 = list_nationsItem_create(2);
                        ^
  30|   _t1.data[0] = (nationsItem){.id = 1, .name = "A"};
  31|   _t1.data[1] = (nationsItem){.id = 2, .name = "B"};
line 41:64: function definition is not allowed here
  40|   } list_suppliersItem;
  41|   static list_suppliersItem list_suppliersItem_create(int len) {
                                                                    ^
  42|     list_suppliersItem l;
  43|     l.len = len;
line 47:28: call to undeclared function 'list_suppliersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  46|   }
  47|   list_suppliersItem _t2 = list_suppliersItem_create(2);
                                ^
  48|   _t2.data[0] = (suppliersItem){.id = 1, .nation = 1};
  49|   _t2.data[1] = (suppliersItem){.id = 2, .nation = 2};
line 47:22: initializing 'list_suppliersItem' with an expression of incompatible type 'int'
  46|   }
  47|   list_suppliersItem _t2 = list_suppliersItem_create(2);
                          ^
  48|   _t2.data[0] = (suppliersItem){.id = 1, .nation = 1};
  49|   _t2.data[1] = (suppliersItem){.id = 2, .nation = 2};
line 61:62: function definition is not allowed here
  60|   } list_partsuppItem;
  61|   static list_partsuppItem list_partsuppItem_create(int len) {
                                                                  ^
  62|     list_partsuppItem l;
  63|     l.len = len;
line 67:27: call to undeclared function 'list_partsuppItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  66|   }
  67|   list_partsuppItem _t3 = list_partsuppItem_create(3);
                               ^
  68|   _t3.data[0] =
  69|       (partsuppItem){.part = 100, .supplier = 1, .cost = 10.0, .qty = 2};
line 67:21: initializing 'list_partsuppItem' with an expression of incompatible type 'int'
  66|   }
  67|   list_partsuppItem _t3 = list_partsuppItem_create(3);
                         ^
  68|   _t3.data[0] =
  69|       (partsuppItem){.part = 100, .supplier = 1, .cost = 10.0, .qty = 2};
line 75:12: initializing 'list_int' with an expression of incompatible type 'int'
  74|   list_partsuppItem partsupp = _t3;
  75|   list_int filtered = 0;
                ^
  76|   list_int grouped = 0;
  77|   printf("%d\n", grouped);
line 76:12: initializing 'list_int' with an expression of incompatible type 'int'
  75|   list_int filtered = 0;
  76|   list_int grouped = 0;
                ^
  77|   printf("%d\n", grouped);
  78|   return 0;

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
 10:   l.len = len;exit status 1

- group_by_multi_join_sort: convert error: exit status 1
line 23:58: function definition is not allowed here
  22|   } list_nationItem;
  23|   static list_nationItem list_nationItem_create(int len) {
                                                              ^
  24|     list_nationItem l;
  25|     l.len = len;
line 29:25: call to undeclared function 'list_nationItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_nationItem _t1 = list_nationItem_create(1);
                             ^
  30|   _t1.data[0] = (nationItem){.n_nationkey = 1, .n_name = "BRAZIL"};
  31|   list_nationItem nation = _t1;
line 29:19: initializing 'list_nationItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_nationItem _t1 = list_nationItem_create(1);
                       ^
  30|   _t1.data[0] = (nationItem){.n_nationkey = 1, .n_name = "BRAZIL"};
  31|   list_nationItem nation = _t1;
line 45:62: function definition is not allowed here
  44|   } list_customerItem;
  45|   static list_customerItem list_customerItem_create(int len) {
                                                                  ^
  46|     list_customerItem l;
  47|     l.len = len;
line 51:27: call to undeclared function 'list_customerItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  50|   }
  51|   list_customerItem _t2 = list_customerItem_create(1);
                               ^
  52|   _t2.data[0] = (customerItem){.c_custkey = 1,
  53|                                .c_name = "Alice",
line 51:21: initializing 'list_customerItem' with an expression of incompatible type 'int'
  50|   }
  51|   list_customerItem _t2 = list_customerItem_create(1);
                         ^
  52|   _t2.data[0] = (customerItem){.c_custkey = 1,
  53|                                .c_name = "Alice",
line 69:58: function definition is not allowed here
  68|   } list_ordersItem;
  69|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  70|     list_ordersItem l;
  71|     l.len = len;
line 75:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  74|   }
  75|   list_ordersItem _t3 = list_ordersItem_create(2);
                             ^
  76|   _t3.data[0] = (ordersItem){
  77|       .o_orderkey = 1000, .o_custkey = 1, .o_orderdate = "1993-10-15"};
line 75:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  74|   }
  75|   list_ordersItem _t3 = list_ordersItem_create(2);
                       ^
  76|   _t3.data[0] = (ordersItem){
  77|       .o_orderkey = 1000, .o_custkey = 1, .o_orderdate = "1993-10-15"};
line 91:62: function definition is not allowed here
  90|   } list_lineitemItem;
  91|   static list_lineitemItem list_lineitemItem_create(int len) {
                                                                  ^
  92|     list_lineitemItem l;
  93|     l.len = len;
line 97:27: call to undeclared function 'list_lineitemItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  96|   }
  97|   list_lineitemItem _t4 = list_lineitemItem_create(2);
                               ^
  98|   _t4.data[0] = (lineitemItem){.l_orderkey = 1000,
  99|                                .l_returnflag = "R",
line 97:21: initializing 'list_lineitemItem' with an expression of incompatible type 'int'
  96|   }
  97|   list_lineitemItem _t4 = list_lineitemItem_create(2);
                         ^
  98|   _t4.data[0] = (lineitemItem){.l_orderkey = 1000,
  99|                                .l_returnflag = "R",

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
 10:   l.len = len;exit status 1

- group_by_sort: convert error: exit status 1
line 23:56: function definition is not allowed here
  22|   } list_itemsItem;
  23|   static list_itemsItem list_itemsItem_create(int len) {
                                                            ^
  24|     list_itemsItem l;
  25|     l.len = len;
line 29:24: call to undeclared function 'list_itemsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_itemsItem _t1 = list_itemsItem_create(4);
                            ^
  30|   _t1.data[0] = (itemsItem){.cat = "a", .val = 3};
  31|   _t1.data[1] = (itemsItem){.cat = "a", .val = 1};
line 29:18: initializing 'list_itemsItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_itemsItem _t1 = list_itemsItem_create(4);
                      ^
  30|   _t1.data[0] = (itemsItem){.cat = "a", .val = 3};
  31|   _t1.data[1] = (itemsItem){.cat = "a", .val = 1};
line 35:12: initializing 'list_int' with an expression of incompatible type 'int'
  34|   list_itemsItem items = _t1;
  35|   list_int grouped = 0;
                ^
  36|   printf("%d\n", grouped);
  37|   return 0;

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
 10:   l.len = len;exit status 1

- group_items_iteration: convert error: exit status 1
line 66:54: function definition is not allowed here
  65|   } list_dataItem;
  66|   static list_dataItem list_dataItem_create(int len) {
                                                          ^
  67|     list_dataItem l;
  68|     l.len = len;
line 72:23: call to undeclared function 'list_dataItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  71|   }
  72|   list_dataItem _t1 = list_dataItem_create(3);
                           ^
  73|   _t1.data[0] = (dataItem){.tag = "a", .val = 1};
  74|   _t1.data[1] = (dataItem){.tag = "a", .val = 2};
line 72:17: initializing 'list_dataItem' with an expression of incompatible type 'int'
  71|   }
  72|   list_dataItem _t1 = list_dataItem_create(3);
                     ^
  73|   _t1.data[0] = (dataItem){.tag = "a", .val = 1};
  74|   _t1.data[1] = (dataItem){.tag = "a", .val = 2};
line 77:12: initializing 'list_int' with an expression of incompatible type 'int'
  76|   list_dataItem data = _t1;
  77|   list_int groups = 0;
                ^
  78|   list_int tmp = _t2;
  79|   for (int _t3 = 0; _t3 < groups.len; _t3++) {
line 78:18: use of undeclared identifier '_t2'; did you mean '_t1'?
  77|   list_int groups = 0;
  78|   list_int tmp = _t2;
                      ^
  79|   for (int _t3 = 0; _t3 < groups.len; _t3++) {
  80|     int g = groups.data[_t3];
line 78:12: initializing 'list_int' with an expression of incompatible type 'list_dataItem'
  77|   list_int groups = 0;
  78|   list_int tmp = _t2;
                ^
  79|   for (int _t3 = 0; _t3 < groups.len; _t3++) {
  80|     int g = groups.data[_t3];
line 82:30: member reference base type 'int' is not a structure or union
  81|     int total = 0;
  82|     for (int _t4 = 0; _t4 < g.items.len; _t4++) {
                                  ^
  83|       int x = g.items.data[_t4];
  84|       total = (total + x.val);
line 83:16: member reference base type 'int' is not a structure or union
  82|     for (int _t4 = 0; _t4 < g.items.len; _t4++) {
  83|       int x = g.items.data[_t4];
                    ^
  84|       total = (total + x.val);
  85|     }
line 84:25: member reference base type 'int' is not a structure or union
  83|       int x = g.items.data[_t4];
  84|       total = (total + x.val);
                             ^
  85|     }
  86|     map_int_bool _t5 = map_int_bool_create(2);
line 87:34: member reference base type 'int' is not a structure or union
  86|     map_int_bool _t5 = map_int_bool_create(2);
  87|     map_int_bool_put(&_t5, tag, g.key);
                                      ^
  88|     map_int_bool_put(&_t5, total, total);
  89|     tmp = append(tmp, _t5);
line 87:28: use of undeclared identifier 'tag'
  86|     map_int_bool _t5 = map_int_bool_create(2);
  87|     map_int_bool_put(&_t5, tag, g.key);
                                ^
  88|     map_int_bool_put(&_t5, total, total);
  89|     tmp = append(tmp, _t5);
line 89:11: call to undeclared function 'append'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  88|     map_int_bool_put(&_t5, total, total);
  89|     tmp = append(tmp, _t5);
               ^
  90|   }
  91|   list_int _t6 = list_int_create(tmp.len);
line 89:9: assigning to 'list_int' from incompatible type 'int'
  88|     map_int_bool_put(&_t5, total, total);
  89|     tmp = append(tmp, _t5);
             ^
  90|   }
  91|   list_int _t6 = list_int_create(tmp.len);
line 97:17: member reference base type 'int' is not a structure or union
  96|     _t6.data[_t7] = r;
  97|     _t9[_t7] = r.tag;
                     ^
  98|     _t7++;
  99|   }
line 115:4: expected '}'
 114|     return 0;
 115|   }
        ^
 116|

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
 10:   l.len = len;exit status 1

- if_else: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- if_then_else: parse roundtrip error: parse error: 7:22: lexer: invalid input text "? \"yes\" : \"no\")\n..."
- if_then_else_nested: parse roundtrip error: parse error: 7:22: lexer: invalid input text "? \"big\" : ((x > ..."
- in_operator: parse roundtrip error: parse error: 7:17: lexer: invalid input text "; i < v.len; i++..."
- in_operator_extended: convert error: exit status 1
line 88:26: use of undeclared identifier 'a'
  87|   map_int_bool _t5 = map_int_bool_create(1);
  88|   map_int_bool_put(&_t5, a, 1);
                              ^
  89|   int m = _t5;
  90|   printf("%d\n", ("a" in m));
line 89:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  88|   map_int_bool_put(&_t5, a, 1);
  89|   int m = _t5;
           ^
  90|   printf("%d\n", ("a" in m));
  91|   printf("%d\n", ("b" in m));
line 90:23: expected ')'
  89|   int m = _t5;
  90|   printf("%d\n", ("a" in m));
                           ^
  91|   printf("%d\n", ("b" in m));
  92|   char *s = "hello";
line 91:23: expected ')'
  90|   printf("%d\n", ("a" in m));
  91|   printf("%d\n", ("b" in m));
                           ^
  92|   char *s = "hello";
  93|   printf("%d\n", contains_string(s, "ell"));

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
 10:   list_int l;exit status 1

- inner_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(3);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(3);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 43:58: function definition is not allowed here
  42|   } list_ordersItem;
  43|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  44|     list_ordersItem l;
  45|     l.len = len;
line 49:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  48|   }
  49|   list_ordersItem _t2 = list_ordersItem_create(4);
                             ^
  50|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  51|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 49:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  48|   }
  49|   list_ordersItem _t2 = list_ordersItem_create(4);
                       ^
  50|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  51|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 55:12: initializing 'list_int' with an expression of incompatible type 'int'
  54|   list_ordersItem orders = _t2;
  55|   list_int result = 0;
                ^
  56|   printf("%s\n", "--- Orders with customer info ---");
  57|   for (int _t3 = 0; _t3 < result.len; _t3++) {
line 60:24: member reference base type 'int' is not a structure or union
  59|     printf("%s ", "Order");
  60|     printf("%d ", entry.orderId);
                            ^
  61|     printf("%s ", "by");
  62|     printf("%d ", entry.customerName);
line 62:24: member reference base type 'int' is not a structure or union
  61|     printf("%s ", "by");
  62|     printf("%d ", entry.customerName);
                            ^
  63|     printf("%s ", "- $");
  64|     printf("%d\n", entry.total);
line 64:25: member reference base type 'int' is not a structure or union
  63|     printf("%s ", "- $");
  64|     printf("%d\n", entry.total);
                             ^
  65|   }
  66|   return 0;

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
 10:   l.len = len;exit status 1

- join_multi: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 41:58: function definition is not allowed here
  40|   } list_ordersItem;
  41|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  42|     list_ordersItem l;
  43|     l.len = len;
line 47:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  46|   }
  47|   list_ordersItem _t2 = list_ordersItem_create(2);
                             ^
  48|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  49|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2};
line 47:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  46|   }
  47|   list_ordersItem _t2 = list_ordersItem_create(2);
                       ^
  48|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  49|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2};
line 59:56: function definition is not allowed here
  58|   } list_itemsItem;
  59|   static list_itemsItem list_itemsItem_create(int len) {
                                                            ^
  60|     list_itemsItem l;
  61|     l.len = len;
line 65:24: call to undeclared function 'list_itemsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  64|   }
  65|   list_itemsItem _t3 = list_itemsItem_create(2);
                            ^
  66|   _t3.data[0] = (itemsItem){.orderId = 100, .sku = "a"};
  67|   _t3.data[1] = (itemsItem){.orderId = 101, .sku = "b"};
line 65:18: initializing 'list_itemsItem' with an expression of incompatible type 'int'
  64|   }
  65|   list_itemsItem _t3 = list_itemsItem_create(2);
                      ^
  66|   _t3.data[0] = (itemsItem){.orderId = 100, .sku = "a"};
  67|   _t3.data[1] = (itemsItem){.orderId = 101, .sku = "b"};
line 69:12: initializing 'list_int' with an expression of incompatible type 'int'
  68|   list_itemsItem items = _t3;
  69|   list_int result = 0;
                ^
  70|   printf("%s\n", "--- Multi Join ---");
  71|   for (int _t4 = 0; _t4 < result.len; _t4++) {
line 73:20: member reference base type 'int' is not a structure or union
  72|     int r = result.data[_t4];
  73|     printf("%d ", r.name);
                        ^
  74|     printf("%s ", "bought item");
  75|     printf("%d\n", r.sku);
line 75:21: member reference base type 'int' is not a structure or union
  74|     printf("%s ", "bought item");
  75|     printf("%d\n", r.sku);
                         ^
  76|   }
  77|   return 0;

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
 10:   l.len = len;exit status 1

- json_builtin: convert error: exit status 1
line 129:26: use of undeclared identifier 'a'
 128|   map_int_bool _t1 = map_int_bool_create(2);
 129|   map_int_bool_put(&_t1, a, 1);
                              ^
 130|   map_int_bool_put(&_t1, b, 2);
 131|   int m = _t1;
line 130:26: use of undeclared identifier 'b'
 129|   map_int_bool_put(&_t1, a, 1);
 130|   map_int_bool_put(&_t1, b, 2);
                              ^
 131|   int m = _t1;
 132|   _json_int(m);
line 131:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
 130|   map_int_bool_put(&_t1, b, 2);
 131|   int m = _t1;
           ^
 132|   _json_int(m);
 133|   return 0;

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
 10:   list_int l;exit status 1

- left_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 42:58: function definition is not allowed here
  41|   } list_ordersItem;
  42|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  43|     list_ordersItem l;
  44|     l.len = len;
line 48:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  47|   }
  48|   list_ordersItem _t2 = list_ordersItem_create(2);
                             ^
  49|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  50|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 3, .total = 80};
line 48:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  47|   }
  48|   list_ordersItem _t2 = list_ordersItem_create(2);
                       ^
  49|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  50|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 3, .total = 80};
line 52:12: initializing 'list_int' with an expression of incompatible type 'int'
  51|   list_ordersItem orders = _t2;
  52|   list_int result = 0;
                ^
  53|   printf("%s\n", "--- Left Join ---");
  54|   for (int _t3 = 0; _t3 < result.len; _t3++) {
line 57:24: member reference base type 'int' is not a structure or union
  56|     printf("%s ", "Order");
  57|     printf("%d ", entry.orderId);
                            ^
  58|     printf("%s ", "customer");
  59|     printf("%d ", entry.customer);
line 59:24: member reference base type 'int' is not a structure or union
  58|     printf("%s ", "customer");
  59|     printf("%d ", entry.customer);
                            ^
  60|     printf("%s ", "total");
  61|     printf("%d\n", entry.total);
line 61:25: member reference base type 'int' is not a structure or union
  60|     printf("%s ", "total");
  61|     printf("%d\n", entry.total);
                             ^
  62|   }
  63|   return 0;

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
 10:   l.len = len;exit status 1

- left_join_multi: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(2);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 41:58: function definition is not allowed here
  40|   } list_ordersItem;
  41|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  42|     list_ordersItem l;
  43|     l.len = len;
line 47:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  46|   }
  47|   list_ordersItem _t2 = list_ordersItem_create(2);
                             ^
  48|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  49|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2};
line 47:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  46|   }
  47|   list_ordersItem _t2 = list_ordersItem_create(2);
                       ^
  48|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1};
  49|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2};
line 59:56: function definition is not allowed here
  58|   } list_itemsItem;
  59|   static list_itemsItem list_itemsItem_create(int len) {
                                                            ^
  60|     list_itemsItem l;
  61|     l.len = len;
line 65:24: call to undeclared function 'list_itemsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  64|   }
  65|   list_itemsItem _t3 = list_itemsItem_create(1);
                            ^
  66|   _t3.data[0] = (itemsItem){.orderId = 100, .sku = "a"};
  67|   list_itemsItem items = _t3;
line 65:18: initializing 'list_itemsItem' with an expression of incompatible type 'int'
  64|   }
  65|   list_itemsItem _t3 = list_itemsItem_create(1);
                      ^
  66|   _t3.data[0] = (itemsItem){.orderId = 100, .sku = "a"};
  67|   list_itemsItem items = _t3;
line 68:12: initializing 'list_int' with an expression of incompatible type 'int'
  67|   list_itemsItem items = _t3;
  68|   list_int result = 0;
                ^
  69|   printf("%s\n", "--- Left Join Multi ---");
  70|   for (int _t4 = 0; _t4 < result.len; _t4++) {
line 72:20: member reference base type 'int' is not a structure or union
  71|     int r = result.data[_t4];
  72|     printf("%d ", r.orderId);
                        ^
  73|     printf("%d ", r.name);
  74|     printf("%d\n", r.item);
line 73:20: member reference base type 'int' is not a structure or union
  72|     printf("%d ", r.orderId);
  73|     printf("%d ", r.name);
                        ^
  74|     printf("%d\n", r.item);
  75|   }
line 74:21: member reference base type 'int' is not a structure or union
  73|     printf("%d ", r.name);
  74|     printf("%d\n", r.item);
                         ^
  75|   }
  76|   return 0;

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
 10:   l.len = len;exit status 1

- len_builtin: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- len_map: convert error: exit status 1
line 59:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(2);
  59|   map_int_bool_put(&_t1, "a", 1);
                              ^
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   printf("%d\n", _t1.len);
line 60:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, "a", 1);
  60|   map_int_bool_put(&_t1, "b", 2);
                              ^
  61|   printf("%d\n", _t1.len);
  62|   return 0;

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
 10:   l.len = len;exit status 1

- len_string: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- let_and_print: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- list_assign: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- list_index: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- list_nested_assign: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- list_set_ops: parse roundtrip error: parse error: 15:17: lexer: invalid input text "; i < a.len; i++..."
- load_yaml: convert error: exit status 1
line 227:3: typedef redefinition with different types ('struct Person' (aka 'Person') vs 'struct Person')
 226|   char *email;
 227| } Person;
       ^
 228| 
 229| int main() {
line 230:3: use of undeclared identifier 'list_Person'
 229| int main() {
 230|   list_Person people = _load_json("../interpreter/valid/people.yaml");
       ^
 231|   map_int_bool _t1 = map_int_bool_create(2);
 232|   map_int_bool_put(&_t1, name, p.name);
line 232:26: use of undeclared identifier 'name'
 231|   map_int_bool _t1 = map_int_bool_create(2);
 232|   map_int_bool_put(&_t1, name, p.name);
                              ^
 233|   map_int_bool_put(&_t1, email, p.email);
 234|   list_int _t2 = list_int_create(people.len);
line 232:32: use of undeclared identifier 'p'
 231|   map_int_bool _t1 = map_int_bool_create(2);
 232|   map_int_bool_put(&_t1, name, p.name);
                                    ^
 233|   map_int_bool_put(&_t1, email, p.email);
 234|   list_int _t2 = list_int_create(people.len);
line 233:26: use of undeclared identifier 'email'
 232|   map_int_bool_put(&_t1, name, p.name);
 233|   map_int_bool_put(&_t1, email, p.email);
                              ^
 234|   list_int _t2 = list_int_create(people.len);
 235|   int _t3 = 0;
line 233:33: use of undeclared identifier 'p'
 232|   map_int_bool_put(&_t1, name, p.name);
 233|   map_int_bool_put(&_t1, email, p.email);
                                     ^
 234|   list_int _t2 = list_int_create(people.len);
 235|   int _t3 = 0;
line 234:34: use of undeclared identifier 'people'
 233|   map_int_bool_put(&_t1, email, p.email);
 234|   list_int _t2 = list_int_create(people.len);
                                      ^
 235|   int _t3 = 0;
 236|   for (int _t4 = 0; _t4 < people.len; _t4++) {
line 236:27: use of undeclared identifier 'people'
 235|   int _t3 = 0;
 236|   for (int _t4 = 0; _t4 < people.len; _t4++) {
                               ^
 237|     Person p = people.data[_t4];
 238|     if (!((p.age >= 18))) {
line 237:16: use of undeclared identifier 'people'
 236|   for (int _t4 = 0; _t4 < people.len; _t4++) {
 237|     Person p = people.data[_t4];
                    ^
 238|     if (!((p.age >= 18))) {
 239|       continue;
line 237:12: variable has incomplete type 'Person' (aka 'struct Person')
 236|   for (int _t4 = 0; _t4 < people.len; _t4++) {
 237|     Person p = people.data[_t4];
                ^
 238|     if (!((p.age >= 18))) {
 239|       continue;
line 241:19: assigning to 'int' from incompatible type 'map_int_bool'
 240|     }
 241|     _t2.data[_t3] = _t1;
                       ^
 242|     _t3++;
 243|   }
line 248:20: member reference base type 'int' is not a structure or union
 247|     int a = adults.data[_t5];
 248|     printf("%d ", a.name);
                        ^
 249|     printf("%d\n", a.email);
 250|   }
line 249:21: member reference base type 'int' is not a structure or union
 248|     printf("%d ", a.name);
 249|     printf("%d\n", a.email);
                         ^
 250|   }
 251|   return 0;

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
 10:   list_int l;exit status 1

- map_assign: convert error: exit status 1
line 59:26: incompatible pointer to integer conversion passing 'char[6]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(1);
  59|   map_int_bool_put(&_t1, "alice", 1);
                              ^
  60|   int scores = _t1;
  61|   scores.data["bob"] = 2;
line 60:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  59|   map_int_bool_put(&_t1, "alice", 1);
  60|   int scores = _t1;
           ^
  61|   scores.data["bob"] = 2;
  62|   printf("%d\n", scores.data["bob"]);
line 61:9: member reference base type 'int' is not a structure or union
  60|   int scores = _t1;
  61|   scores.data["bob"] = 2;
             ^
  62|   printf("%d\n", scores.data["bob"]);
  63|   return 0;
line 61:14: array subscript is not an integer
  60|   int scores = _t1;
  61|   scores.data["bob"] = 2;
                  ^
  62|   printf("%d\n", scores.data["bob"]);
  63|   return 0;
line 62:24: member reference base type 'int' is not a structure or union
  61|   scores.data["bob"] = 2;
  62|   printf("%d\n", scores.data["bob"]);
                            ^
  63|   return 0;
  64| }
line 62:29: array subscript is not an integer
  61|   scores.data["bob"] = 2;
  62|   printf("%d\n", scores.data["bob"]);
                                 ^
  63|   return 0;
  64| }

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
 10:   l.len = len;exit status 1

- map_in_operator: convert error: exit status 1
line 59:29: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(2);
  59|   map_int_bool_put(&_t1, 1, "a");
                                 ^
  60|   map_int_bool_put(&_t1, 2, "b");
  61|   int m = _t1;
line 60:29: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, 1, "a");
  60|   map_int_bool_put(&_t1, 2, "b");
                                 ^
  61|   int m = _t1;
  62|   printf("%d\n", (1 in m));
line 61:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  60|   map_int_bool_put(&_t1, 2, "b");
  61|   int m = _t1;
           ^
  62|   printf("%d\n", (1 in m));
  63|   printf("%d\n", (3 in m));
line 62:21: expected ')'
  61|   int m = _t1;
  62|   printf("%d\n", (1 in m));
                         ^
  63|   printf("%d\n", (3 in m));
  64|   return 0;
line 63:21: expected ')'
  62|   printf("%d\n", (1 in m));
  63|   printf("%d\n", (3 in m));
                         ^
  64|   return 0;
  65| }

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
 10:   l.len = len;exit status 1

- map_index: convert error: exit status 1
line 59:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(2);
  59|   map_int_bool_put(&_t1, "a", 1);
                              ^
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   int m = _t1;
line 60:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, "a", 1);
  60|   map_int_bool_put(&_t1, "b", 2);
                              ^
  61|   int m = _t1;
  62|   printf("%d\n", m.data["b"]);
line 61:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   int m = _t1;
           ^
  62|   printf("%d\n", m.data["b"]);
  63|   return 0;
line 62:19: member reference base type 'int' is not a structure or union
  61|   int m = _t1;
  62|   printf("%d\n", m.data["b"]);
                       ^
  63|   return 0;
  64| }
line 62:24: array subscript is not an integer
  61|   int m = _t1;
  62|   printf("%d\n", m.data["b"]);
                            ^
  63|   return 0;
  64| }

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
 10:   l.len = len;exit status 1

- map_int_key: convert error: exit status 1
line 59:29: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(2);
  59|   map_int_bool_put(&_t1, 1, "a");
                                 ^
  60|   map_int_bool_put(&_t1, 2, "b");
  61|   int m = _t1;
line 60:29: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, 1, "a");
  60|   map_int_bool_put(&_t1, 2, "b");
                                 ^
  61|   int m = _t1;
  62|   printf("%s\n", m.data[1]);
line 61:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  60|   map_int_bool_put(&_t1, 2, "b");
  61|   int m = _t1;
           ^
  62|   printf("%s\n", m.data[1]);
  63|   return 0;
line 62:19: member reference base type 'int' is not a structure or union
  61|   int m = _t1;
  62|   printf("%s\n", m.data[1]);
                       ^
  63|   return 0;
  64| }

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
 10:   l.len = len;exit status 1

- map_literal_dynamic: convert error: exit status 1
line 61:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  60|   map_int_bool _t1 = map_int_bool_create(2);
  61|   map_int_bool_put(&_t1, "a", x);
                              ^
  62|   map_int_bool_put(&_t1, "b", y);
  63|   int m = _t1;
line 62:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  61|   map_int_bool_put(&_t1, "a", x);
  62|   map_int_bool_put(&_t1, "b", y);
                              ^
  63|   int m = _t1;
  64|   printf("%d ", m.data["a"]);
line 63:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  62|   map_int_bool_put(&_t1, "b", y);
  63|   int m = _t1;
           ^
  64|   printf("%d ", m.data["a"]);
  65|   printf("%d\n", m.data["b"]);
line 64:18: member reference base type 'int' is not a structure or union
  63|   int m = _t1;
  64|   printf("%d ", m.data["a"]);
                      ^
  65|   printf("%d\n", m.data["b"]);
  66|   return 0;
line 64:23: array subscript is not an integer
  63|   int m = _t1;
  64|   printf("%d ", m.data["a"]);
                           ^
  65|   printf("%d\n", m.data["b"]);
  66|   return 0;
line 65:19: member reference base type 'int' is not a structure or union
  64|   printf("%d ", m.data["a"]);
  65|   printf("%d\n", m.data["b"]);
                       ^
  66|   return 0;
  67| }
line 65:24: array subscript is not an integer
  64|   printf("%d ", m.data["a"]);
  65|   printf("%d\n", m.data["b"]);
                            ^
  66|   return 0;
  67| }

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
 10:   l.len = len;exit status 1

- map_membership: convert error: exit status 1
line 59:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(2);
  59|   map_int_bool_put(&_t1, "a", 1);
                              ^
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   int m = _t1;
line 60:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, "a", 1);
  60|   map_int_bool_put(&_t1, "b", 2);
                              ^
  61|   int m = _t1;
  62|   printf("%d\n", ("a" in m));
line 61:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   int m = _t1;
           ^
  62|   printf("%d\n", ("a" in m));
  63|   printf("%d\n", ("c" in m));
line 62:23: expected ')'
  61|   int m = _t1;
  62|   printf("%d\n", ("a" in m));
                           ^
  63|   printf("%d\n", ("c" in m));
  64|   return 0;
line 63:23: expected ')'
  62|   printf("%d\n", ("a" in m));
  63|   printf("%d\n", ("c" in m));
                           ^
  64|   return 0;
  65| }

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
 10:   l.len = len;exit status 1

- map_nested_assign: convert error: exit status 1
line 60:26: incompatible pointer to integer conversion passing 'char[6]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool _t2 = map_int_bool_create(1);
  60|   map_int_bool_put(&_t2, "inner", 1);
                              ^
  61|   map_int_bool_put(&_t1, "outer", _t2);
  62|   int data = _t1;
line 61:26: incompatible pointer to integer conversion passing 'char[6]' to parameter of type 'int' [-Wint-conversion]
  60|   map_int_bool_put(&_t2, "inner", 1);
  61|   map_int_bool_put(&_t1, "outer", _t2);
                              ^
  62|   int data = _t1;
  63|   data.data["outer"].data["inner"] = 2;
line 61:35: passing 'map_int_bool' to parameter of incompatible type 'int'
  60|   map_int_bool_put(&_t2, "inner", 1);
  61|   map_int_bool_put(&_t1, "outer", _t2);
                                       ^
  62|   int data = _t1;
  63|   data.data["outer"].data["inner"] = 2;
line 62:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  61|   map_int_bool_put(&_t1, "outer", _t2);
  62|   int data = _t1;
           ^
  63|   data.data["outer"].data["inner"] = 2;
  64|   printf("%d\n", data.data["outer"].data["inner"]);
line 63:7: member reference base type 'int' is not a structure or union
  62|   int data = _t1;
  63|   data.data["outer"].data["inner"] = 2;
           ^
  64|   printf("%d\n", data.data["outer"].data["inner"]);
  65|   return 0;
line 63:12: array subscript is not an integer
  62|   int data = _t1;
  63|   data.data["outer"].data["inner"] = 2;
                ^
  64|   printf("%d\n", data.data["outer"].data["inner"]);
  65|   return 0;
line 64:22: member reference base type 'int' is not a structure or union
  63|   data.data["outer"].data["inner"] = 2;
  64|   printf("%d\n", data.data["outer"].data["inner"]);
                          ^
  65|   return 0;
  66| }
line 64:27: array subscript is not an integer
  63|   data.data["outer"].data["inner"] = 2;
  64|   printf("%d\n", data.data["outer"].data["inner"]);
                               ^
  65|   return 0;
  66| }

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
 10:   l.len = len;exit status 1

- match_expr: parse roundtrip error: parse error: 8:9: lexer: invalid input text "? \"one\" : (x == ..."
- match_full: parse roundtrip error: parse error: 7:18: lexer: invalid input text "? \"zero\" : (n ==..."
- math_ops: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- membership: parse roundtrip error: parse error: 7:17: lexer: invalid input text "; i < v.len; i++..."
- min_max_builtin: parse roundtrip error: parse error: 12:18: lexer: invalid input text "? nums.data[0] :..."
- nested_function: convert error: exit status 1
line 14:28: use of undeclared identifier 'x'
  13| }
  14| int inner(int y) { return (x + y); }
                                ^
  15| 
  16| int outer(int x) { return inner(5); }

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
 10:   l.len = len;exit status 1

- order_by_map: convert error: exit status 1
line 66:54: function definition is not allowed here
  65|   } list_dataItem;
  66|   static list_dataItem list_dataItem_create(int len) {
                                                          ^
  67|     list_dataItem l;
  68|     l.len = len;
line 72:23: call to undeclared function 'list_dataItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  71|   }
  72|   list_dataItem _t1 = list_dataItem_create(3);
                           ^
  73|   _t1.data[0] = (dataItem){.a = 1, .b = 2};
  74|   _t1.data[1] = (dataItem){.a = 1, .b = 1};
line 72:17: initializing 'list_dataItem' with an expression of incompatible type 'int'
  71|   }
  72|   list_dataItem _t1 = list_dataItem_create(3);
                     ^
  73|   _t1.data[0] = (dataItem){.a = 1, .b = 2};
  74|   _t1.data[1] = (dataItem){.a = 1, .b = 1};
line 78:26: use of undeclared identifier 'a'
  77|   map_int_bool _t2 = map_int_bool_create(2);
  78|   map_int_bool_put(&_t2, a, x.a);
                              ^
  79|   map_int_bool_put(&_t2, b, x.b);
  80|   list_dataItem _t3 = list_dataItem_create(data.len);
line 78:29: use of undeclared identifier 'x'
  77|   map_int_bool _t2 = map_int_bool_create(2);
  78|   map_int_bool_put(&_t2, a, x.a);
                                 ^
  79|   map_int_bool_put(&_t2, b, x.b);
  80|   list_dataItem _t3 = list_dataItem_create(data.len);
line 79:26: use of undeclared identifier 'b'
  78|   map_int_bool_put(&_t2, a, x.a);
  79|   map_int_bool_put(&_t2, b, x.b);
                              ^
  80|   list_dataItem _t3 = list_dataItem_create(data.len);
  81|   int *_t6 = (int *)malloc(sizeof(int) * data.len);
line 79:29: use of undeclared identifier 'x'
  78|   map_int_bool_put(&_t2, a, x.a);
  79|   map_int_bool_put(&_t2, b, x.b);
                                 ^
  80|   list_dataItem _t3 = list_dataItem_create(data.len);
  81|   int *_t6 = (int *)malloc(sizeof(int) * data.len);
line 80:17: initializing 'list_dataItem' with an expression of incompatible type 'int'
  79|   map_int_bool_put(&_t2, b, x.b);
  80|   list_dataItem _t3 = list_dataItem_create(data.len);
                     ^
  81|   int *_t6 = (int *)malloc(sizeof(int) * data.len);
  82|   int _t4 = 0;
line 86:14: assigning to 'int' from incompatible type 'map_int_bool'
  85|     _t3.data[_t4] = x;
  86|     _t6[_t4] = _t2;
                  ^
  87|     _t4++;
  88|   }
line 104:4: expected '}'
 103|     return 0;
 104|   }
        ^
 105|

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
 10:   l.len = len;exit status 1

- outer_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(4);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(4);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 44:58: function definition is not allowed here
  43|   } list_ordersItem;
  44|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  45|     list_ordersItem l;
  46|     l.len = len;
line 50:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  49|   }
  50|   list_ordersItem _t2 = list_ordersItem_create(4);
                             ^
  51|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  52|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 50:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  49|   }
  50|   list_ordersItem _t2 = list_ordersItem_create(4);
                       ^
  51|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  52|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 56:12: initializing 'list_int' with an expression of incompatible type 'int'
  55|   list_ordersItem orders = _t2;
  56|   list_int result = 0;
                ^
  57|   printf("%s\n", "--- Outer Join using syntax ---");
  58|   for (int _t3 = 0; _t3 < result.len; _t3++) {
line 60:12: member reference base type 'int' is not a structure or union
  59|     int row = result.data[_t3];
  60|     if (row.order) {
                ^
  61|       if (row.customer) {
  62|         printf("%s ", "Order");
line 61:14: member reference base type 'int' is not a structure or union
  60|     if (row.order) {
  61|       if (row.customer) {
                  ^
  62|         printf("%s ", "Order");
  63|         printf("%d ", row.order.id);
line 63:26: member reference base type 'int' is not a structure or union
  62|         printf("%s ", "Order");
  63|         printf("%d ", row.order.id);
                              ^
  64|         printf("%s ", "by");
  65|         printf("%d ", row.customer.name);
line 65:26: member reference base type 'int' is not a structure or union
  64|         printf("%s ", "by");
  65|         printf("%d ", row.customer.name);
                              ^
  66|         printf("%s ", "- $");
  67|         printf("%d\n", row.order.total);
line 67:27: member reference base type 'int' is not a structure or union
  66|         printf("%s ", "- $");
  67|         printf("%d\n", row.order.total);
                               ^
  68|       } else {
  69|         printf("%s ", "Order");
line 70:26: member reference base type 'int' is not a structure or union
  69|         printf("%s ", "Order");
  70|         printf("%d ", row.order.id);
                              ^
  71|         printf("%s ", "by");
  72|         printf("%s ", "Unknown");
line 74:27: member reference base type 'int' is not a structure or union
  73|         printf("%s ", "- $");
  74|         printf("%d\n", row.order.total);
                               ^
  75|       }
  76|     } else {
line 78:24: member reference base type 'int' is not a structure or union
  77|       printf("%s ", "Customer");
  78|       printf("%d ", row.customer.name);
                            ^
  79|       printf("%s\n", "has no orders");
  80|     }

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
 10:   l.len = len;exit status 1

- partial_application: convert error: exit status 1
line 17:27: too few arguments to function call, expected 2, have 1
  16| int main() {
  17|   int (*add5)(int) = add(5);
                               ^
  18|   printf("%d\n", add5(3));
  19|   return 0;

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
 10:   l.len = len;exit status 1

- print_hello: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- pure_fold: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- pure_global_fold: convert error: exit status 1
line 14:30: use of undeclared identifier 'k'
  13| }
  14| int inc(int x) { return (x + k); }
                                  ^
  15| 
  16| int main() {

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
 10:   l.len = len;exit status 1

- query_sum_select: convert error: exit status 1
line 33:30: passing 'int' to parameter of incompatible type 'list_int'
  32|     }
  33|     _t2.data[_t3] = _sum_int(n);
                                  ^
  34|     _t3++;
  35|   }
line 37:10: initializing 'double' with an expression of incompatible type 'list_int'
  36|   _t2.len = _t3;
  37|   double result = _t2;
              ^
  38|   printf("%g\n", result);
  39|   return 0;

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
 10:   l.len = len;exit status 1

- record_assign: convert error: exit status 1
line 18:3: typedef redefinition with different types ('struct Counter' (aka 'Counter') vs 'struct Counter')
  17|   int n;
  18| } Counter;
       ^
  19| 
  20| void inc(Counter c) { c = (c.n + 1); }
line 20:18: variable has incomplete type 'Counter' (aka 'struct Counter')
  19| 
  20| void inc(Counter c) { c = (c.n + 1); }
                      ^
  21| 
  22| int main() {
line 23:15: variable has incomplete type 'Counter' (aka 'struct Counter')
  22| int main() {
  23|   Counter c = (Counter){.n = 0};
                   ^
  24|   inc(c);
  25|   printf("%d\n", c.n);
line 23:11: variable has incomplete type 'Counter' (aka 'struct Counter')
  22| int main() {
  23|   Counter c = (Counter){.n = 0};
               ^
  24|   inc(c);
  25|   printf("%d\n", c.n);

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
 10:   l.len = len;exit status 1

- right_join: convert error: exit status 1
line 23:64: function definition is not allowed here
  22|   } list_customersItem;
  23|   static list_customersItem list_customersItem_create(int len) {
                                                                    ^
  24|     list_customersItem l;
  25|     l.len = len;
line 29:28: call to undeclared function 'list_customersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(4);
                                ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 29:22: initializing 'list_customersItem' with an expression of incompatible type 'int'
  28|   }
  29|   list_customersItem _t1 = list_customersItem_create(4);
                          ^
  30|   _t1.data[0] = (customersItem){.id = 1, .name = "Alice"};
  31|   _t1.data[1] = (customersItem){.id = 2, .name = "Bob"};
line 44:58: function definition is not allowed here
  43|   } list_ordersItem;
  44|   static list_ordersItem list_ordersItem_create(int len) {
                                                              ^
  45|     list_ordersItem l;
  46|     l.len = len;
line 50:25: call to undeclared function 'list_ordersItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  49|   }
  50|   list_ordersItem _t2 = list_ordersItem_create(3);
                             ^
  51|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  52|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 50:19: initializing 'list_ordersItem' with an expression of incompatible type 'int'
  49|   }
  50|   list_ordersItem _t2 = list_ordersItem_create(3);
                       ^
  51|   _t2.data[0] = (ordersItem){.id = 100, .customerId = 1, .total = 250};
  52|   _t2.data[1] = (ordersItem){.id = 101, .customerId = 2, .total = 125};
line 55:12: initializing 'list_int' with an expression of incompatible type 'int'
  54|   list_ordersItem orders = _t2;
  55|   list_int result = 0;
                ^
  56|   printf("%s\n", "--- Right Join using syntax ---");
  57|   for (int _t3 = 0; _t3 < result.len; _t3++) {
line 59:14: member reference base type 'int' is not a structure or union
  58|     int entry = result.data[_t3];
  59|     if (entry.order) {
                  ^
  60|       printf("%s ", "Customer");
  61|       printf("%d ", entry.customerName);
line 61:26: member reference base type 'int' is not a structure or union
  60|       printf("%s ", "Customer");
  61|       printf("%d ", entry.customerName);
                              ^
  62|       printf("%s ", "has order");
  63|       printf("%d ", entry.order.id);
line 63:26: member reference base type 'int' is not a structure or union
  62|       printf("%s ", "has order");
  63|       printf("%d ", entry.order.id);
                              ^
  64|       printf("%s ", "- $");
  65|       printf("%d\n", entry.order.total);
line 65:27: member reference base type 'int' is not a structure or union
  64|       printf("%s ", "- $");
  65|       printf("%d\n", entry.order.total);
                               ^
  66|     } else {
  67|       printf("%s ", "Customer");
line 68:26: member reference base type 'int' is not a structure or union
  67|       printf("%s ", "Customer");
  68|       printf("%d ", entry.customerName);
                              ^
  69|       printf("%s\n", "has no orders");
  70|     }

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
 10:   l.len = len;exit status 1

- save_jsonl_stdout: convert error: exit status 1
line 28:10: call to undeclared function '_isnum'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  27|   for (const char *p = s; *p; p++)
  28|     if (!_isnum(*p))
              ^
  29|       return 0;
  30|   return 1;
line 32:33: unknown type name 'map_string'
  31| }
  32| static void _write_obj(FILE *f, map_string m) {
                                     ^
  33|   fputc('{', f);
  34|   for (int i = 0; i < m.len; i++) {
line 46:24: unknown type name 'list_map_string'
  45| }
  46| static void _save_json(list_map_string rows, const char *path) {
                            ^
  47|   FILE *f = (!path || path[0] == '\0' || strcmp(path, "-") == 0)
  48|                 ? stdout
line 77:58: function definition is not allowed here
  76|   } list_peopleItem;
  77|   static list_peopleItem list_peopleItem_create(int len) {
                                                              ^
  78|     list_peopleItem l;
  79|     l.len = len;
line 83:25: call to undeclared function 'list_peopleItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  82|   }
  83|   list_peopleItem _t1 = list_peopleItem_create(2);
                             ^
  84|   _t1.data[0] = (peopleItem){.name = "Alice", .age = 30};
  85|   _t1.data[1] = (peopleItem){.name = "Bob", .age = 25};
line 83:19: initializing 'list_peopleItem' with an expression of incompatible type 'int'
  82|   }
  83|   list_peopleItem _t1 = list_peopleItem_create(2);
                       ^
  84|   _t1.data[0] = (peopleItem){.name = "Alice", .age = 30};
  85|   _t1.data[1] = (peopleItem){.name = "Bob", .age = 25};

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
 10:   list_int l;exit status 1

- short_circuit: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- slice: parse roundtrip error: parse error: 61:8: lexer: invalid input text "\\n\")\nlist_int _t..."
- sort_stable: convert error: exit status 1
line 43:56: function definition is not allowed here
  42|   } list_itemsItem;
  43|   static list_itemsItem list_itemsItem_create(int len) {
                                                            ^
  44|     list_itemsItem l;
  45|     l.len = len;
line 49:24: call to undeclared function 'list_itemsItem_create'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  48|   }
  49|   list_itemsItem _t1 = list_itemsItem_create(3);
                            ^
  50|   _t1.data[0] = (itemsItem){.n = 1, .v = "a"};
  51|   _t1.data[1] = (itemsItem){.n = 1, .v = "b"};
line 49:18: initializing 'list_itemsItem' with an expression of incompatible type 'int'
  48|   }
  49|   list_itemsItem _t1 = list_itemsItem_create(3);
                      ^
  50|   _t1.data[0] = (itemsItem){.n = 1, .v = "a"};
  51|   _t1.data[1] = (itemsItem){.n = 1, .v = "b"};
line 79:4: expected '}'
  78|     return 0;
  79|   }
        ^
  80|

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
 10:   list_int l;exit status 1

- str_builtin: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- string_compare: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- string_concat: parse roundtrip error: parse error: 12:22: lexer: invalid input text "'\\0'\n  return bu..."
- string_contains: convert error: exit status 1
line 16:19: member reference base type 'char *' is not a structure or union
  15|   char *s = "catch";
  16|   printf("%d\n", s.contains("cat"));
                       ^
  17|   printf("%d\n", s.contains("dog"));
  18|   return 0;
line 17:19: member reference base type 'char *' is not a structure or union
  16|   printf("%d\n", s.contains("cat"));
  17|   printf("%d\n", s.contains("dog"));
                       ^
  18|   return 0;
  19| }

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
 10:   l.len = len;exit status 1

- string_in_operator: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- string_index: parse roundtrip error: parse error: 18:9: lexer: invalid input text "'\\0'\n_b\n})\nprint..."
- string_prefix_slice: parse roundtrip error: parse error: 24:15: lexer: invalid input text "'\\0'\n_b\n})\nprint..."
- substring_builtin: convert error: exit status 1
line 15:18: call to undeclared function 'substring'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  14| int main() {
  15|   printf("%s\n", substring("mochi", 1, 4));
                      ^
  16|   return 0;
  17| }

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
 10:   l.len = len;exit status 1

- sum_builtin: parse roundtrip error: parse error: 12:15: lexer: invalid input text "; i < _t1.len; i..."
- tail_recursion: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- test_block: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- tree_sum: convert error: exit status 1
line 19:3: typedef redefinition with different types ('struct Leaf' (aka 'Leaf') vs 'struct Leaf')
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
  33| int sum_tree(Tree t) {
line 33:19: variable has incomplete type 'Tree' (aka 'struct Tree')
  32| 
  33| int sum_tree(Tree t) {
                       ^
  34|   return (t == Leaf ? 0
  35|                     : (t == Node(left, value, right)
line 34:16: unexpected type name 'Leaf': expected expression
  33| int sum_tree(Tree t) {
  34|   return (t == Leaf ? 0
                    ^
  35|                     : (t == Node(left, value, right)
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
line 35:29: unexpected type name 'Node': expected expression
  34|   return (t == Leaf ? 0
  35|                     : (t == Node(left, value, right)
                                 ^
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
  37|                            : 0));
line 36:41: use of undeclared identifier 'left'
  35|                     : (t == Node(left, value, right)
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
                                             ^
  37|                            : 0));
  38| }
line 36:49: use of undeclared identifier 'value'
  35|                     : (t == Node(left, value, right)
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
                                                     ^
  37|                            : 0));
  38| }
line 36:67: use of undeclared identifier 'right'
  35|                     : (t == Node(left, value, right)
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
                                                                       ^
  37|                            : 0));
  38| }
line 35:34: use of undeclared identifier 'left'
  34|   return (t == Leaf ? 0
  35|                     : (t == Node(left, value, right)
                                      ^
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
  37|                            : 0));
line 35:40: use of undeclared identifier 'value'
  34|   return (t == Leaf ? 0
  35|                     : (t == Node(left, value, right)
                                            ^
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
  37|                            : 0));
line 35:47: use of undeclared identifier 'right'
  34|   return (t == Leaf ? 0
  35|                     : (t == Node(left, value, right)
                                                   ^
  36|                            ? ((sum_tree(left) + value) + sum_tree(right))
  37|                            : 0));
line 41:26: unexpected type name 'Leaf': expected expression
  40| int main() {
  41|   int t = (Node){.left = Leaf,
                              ^
  42|                  .value = 1,
  43|                  .right = (Node){.left = Leaf, .value = 2, .right = Leaf}};
line 43:42: unexpected type name 'Leaf': expected expression
  42|                  .value = 1,
  43|                  .right = (Node){.left = Leaf, .value = 2, .right = Leaf}};
                                              ^
  44|   printf("%d\n", sum_tree(t));
  45|   return 0;
line 43:69: unexpected type name 'Leaf': expected expression
  42|                  .value = 1,
  43|                  .right = (Node){.left = Leaf, .value = 2, .right = Leaf}};
                                                                         ^
  44|   printf("%d\n", sum_tree(t));
  45|   return 0;

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
 10:   l.len = len;exit status 1

- two-sum: parse roundtrip error: parse error: 8:17: lexer: invalid input text "; i < n; i++) {\n..."
- typed_let: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- typed_var: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- unary_neg: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- update_stmt: convert error: exit status 1
line 20:3: typedef redefinition with different types ('struct Person' (aka 'Person') vs 'struct Person')
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
  55|   printf("%s\n", "ok");

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
 10:   l.len = len;exit status 1

- user_type_literal: convert error: exit status 1
line 20:3: typedef redefinition with different types ('struct Person' (aka 'Person') vs 'struct Person')
  19|   int age;
  20| } Person;
       ^
  21| 
  22| typedef struct {
line 24:10: field has incomplete type 'Person' (aka 'struct Person')
  23|   char *title;
  24|   Person author;
              ^
  25| } Book;
  26| 
line 25:3: typedef redefinition with different types ('struct (unnamed struct at <stdin>:22:9)' vs 'struct Book')
  24|   Person author;
  25| } Book;
       ^
  26| 
  27| int main() {
line 29:39: variable has incomplete type 'Person' (aka 'struct Person')
  28|   Book book =
  29|       (Book){.title = "Go", .author = (Person){.name = "Bob", .age = 42}};
                                           ^
  30|   printf("%s\n", book.author.name);
  31|   return 0;
line 28:8: variable has incomplete type 'Book' (aka 'struct Book')
  27| int main() {
  28|   Book book =
            ^
  29|       (Book){.title = "Go", .author = (Person){.name = "Bob", .age = 42}};
  30|   printf("%s\n", book.author.name);

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
 10:   l.len = len;exit status 1

- values_builtin: convert error: exit status 1
line 59:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  58|   map_int_bool _t1 = map_int_bool_create(3);
  59|   map_int_bool_put(&_t1, "a", 1);
                              ^
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   map_int_bool_put(&_t1, "c", 3);
line 60:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  59|   map_int_bool_put(&_t1, "a", 1);
  60|   map_int_bool_put(&_t1, "b", 2);
                              ^
  61|   map_int_bool_put(&_t1, "c", 3);
  62|   int m = _t1;
line 61:26: incompatible pointer to integer conversion passing 'char[2]' to parameter of type 'int' [-Wint-conversion]
  60|   map_int_bool_put(&_t1, "b", 2);
  61|   map_int_bool_put(&_t1, "c", 3);
                              ^
  62|   int m = _t1;
  63|   printf("%d\n", values(m));
line 62:7: initializing 'int' with an expression of incompatible type 'map_int_bool'
  61|   map_int_bool_put(&_t1, "c", 3);
  62|   int m = _t1;
           ^
  63|   printf("%d\n", values(m));
  64|   return 0;
line 63:18: call to undeclared function 'values'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
  62|   int m = _t1;
  63|   printf("%d\n", values(m));
                      ^
  64|   return 0;
  65| }

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
 10:   l.len = len;exit status 1

- var_assignment: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
- while_loop: parse roundtrip error: parse error: 3:8: unexpected token ":" (expected "}")
