# Errors

- append_builtin: ok
- avg_builtin: ok
- basic_compare: ok
- binary_precedence: ok
- bool_chain: type error: error[T020]: operator `&&` cannot be used on types void and bool
  --> :6:29

help:
  Choose an operator that supports these operand types.
- break_continue: line 3: unsupported line
      1| if (((n % 2) == 0)) {
      2| continue;
>>>   3| }
      4| if ((n > 7)) {
      5| break;

- cast_string_to_int: line 6: unsupported line
      4| }
      5| 
>>>   6| static <T> T _cast(Class<T> cls, Object v) {
      7| if (cls.isInstance(v)) return cls.cast(v);
      8| if (cls == Integer.class) {

- cast_struct: line 17: unsupported line
     15| }
     16| 
>>>  17| static <T> T _cast(Class<T> cls, Object v) {
     18| if (cls.isInstance(v)) return cls.cast(v);
     19| if (cls == Integer.class) {

- closure: line 2: unsupported line
      1| public class Main {
>>>   2| static java.util.function.Function<Integer, Integer> makeAdder(int n) {
      3| return (x) -> {
      4| return (x + n);

- count_builtin: ok
- cross_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1, "total", 250)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 2, "total", 125)), new java.util.HashMap<>(java.util.Map.of("id", 102, "customerId", 1, "total", 300))};
      3| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(orders);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- cross_join_filter: line 4: unsupported line
      2| Object[] letters = new String[]{"A", "B"};
      3| Object[] pairs = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(nums);
      6| _src = _filter(_src, (Object n) -> { return ((n % 2) == 0); });

- cross_join_triple: line 5: unsupported line
      3| Object[] bools = new boolean[]{true, false};
      4| Object[] combos = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   5| public java.util.List<Object> get() {
      6| java.util.List<Object> _src = _toList(nums);
      7| java.util.List<_JoinSpec> _joins = java.util.List.of(

- dataset_sort_take_limit: line 3: unsupported line
      1| Object[] products = new Object[]{new java.util.HashMap<>(java.util.Map.of("name", "Laptop", "price", 1500)), new java.util.HashMap<>(java.util.Map.of("name", "Smartphone", "price", 900)), new java.util.HashMap<>(java.util.Map.of("name", "Tablet", "price", 600)), new java.util.HashMap<>(java.util.Map.of("name", "Monitor", "price", 300)), new java.util.HashMap<>(java.util.Map.of("name", "Keyboard", "price", 100)), new java.util.HashMap<>(java.util.Map.of("name", "Mouse", "price", 50)), new java.util.HashMap<>(java.util.Map.of("name", "Headphones", "price", 200))};
      2| Object[] expensive = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(products);
      5| java.util.List<_JoinSpec> _joins = java.util.List.of(

- dataset_where_filter: line 3: unsupported line
      1| Object[] people = new Object[]{new java.util.HashMap<>(java.util.Map.of("name", "Alice", "age", 30)), new java.util.HashMap<>(java.util.Map.of("name", "Bob", "age", 15)), new java.util.HashMap<>(java.util.Map.of("name", "Charlie", "age", 65)), new java.util.HashMap<>(java.util.Map.of("name", "Diana", "age", 45))};
      2| Object[] adults = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(people);
      5| _src = _filter(_src, (Object person) -> { return (person.get("age") >= 18); });

- exists_builtin: line 3: unsupported line
      1| Object[] data = new int[]{1, 2};
      2| boolean flag = _exists((new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(data);
      5| _src = _filter(_src, (Object x) -> { return (x == 1); });

- for_list_collection: ok
- for_loop: ok
- for_map_collection: ok
- fun_call: ok
- fun_expr_in_let: type error: error[T003]: unknown function: fn
  --> :2:16

help:
  Ensure the function is defined before it's called.
- fun_three_args: ok
- group_by: line 3: unsupported line
      1| Object[] people = new Object[]{new java.util.HashMap<>(java.util.Map.of("name", "Alice", "age", 30, "city", "Paris")), new java.util.HashMap<>(java.util.Map.of("name", "Bob", "age", 15, "city", "Hanoi")), new java.util.HashMap<>(java.util.Map.of("name", "Charlie", "age", 65, "city", "Paris")), new java.util.HashMap<>(java.util.Map.of("name", "Diana", "age", 45, "city", "Hanoi")), new java.util.HashMap<>(java.util.Map.of("name", "Eve", "age", 70, "city", "Paris")), new java.util.HashMap<>(java.util.Map.of("name", "Frank", "age", 22, "city", "Hanoi"))};
      2| Object[] stats = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(people);
      5| java.util.List<_Group> _grps = _group_by(_src, person -> person.get("city"));

- group_by_conditional_sum: line 3: unsupported line
      1| Object[] items = new Object[]{new java.util.HashMap<>(java.util.Map.of("cat", "a", "val", 10, "flag", true)), new java.util.HashMap<>(java.util.Map.of("cat", "a", "val", 5, "flag", false)), new java.util.HashMap<>(java.util.Map.of("cat", "b", "val", 20, "flag", true))};
      2| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(items);
      5| java.util.List<_Group> _grps = _group_by(_src, i -> i.get("cat"));

- group_by_having: line 62: unsupported line
     60| }
     61| 
>>>  62| static java.util.List<_Group> _group_by(
     63| java.util.List<Object> src, java.util.function.Function<Object, Object> keyfn) {
     64| java.util.Map<String, _Group> groups = new java.util.LinkedHashMap<>();

- group_by_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 1)), new java.util.HashMap<>(java.util.Map.of("id", 102, "customerId", 2))};
      3| Object[] stats = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(orders);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- group_by_left_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 1)), new java.util.HashMap<>(java.util.Map.of("id", 102, "customerId", 2))};
      3| Object[] stats = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(customers);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- group_by_multi_join: line 5: unsupported line
      3| Object[] partsupp = new Object[]{new java.util.HashMap<>(java.util.Map.of("part", 100, "supplier", 1, "cost", 10, "qty", 2)), new java.util.HashMap<>(java.util.Map.of("part", 100, "supplier", 2, "cost", 20, "qty", 1)), new java.util.HashMap<>(java.util.Map.of("part", 200, "supplier", 1, "cost", 5, "qty", 3))};
      4| Object[] filtered = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   5| public java.util.List<Object> get() {
      6| java.util.List<Object> _src = _toList(partsupp);
      7| java.util.List<_JoinSpec> _joins = java.util.List.of(

- group_by_multi_join_sort: line 8: unsupported line
      6| String end_date = "1994-01-01";
      7| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   8| public java.util.List<Object> get() {
      9| java.util.List<Object> _src = _toList(customer);
     10| java.util.List<_JoinSpec> _joins = java.util.List.of(

- group_by_sort: line 3: unsupported line
      1| Object[] items = new Object[]{new java.util.HashMap<>(java.util.Map.of("cat", "a", "val", 3)), new java.util.HashMap<>(java.util.Map.of("cat", "a", "val", 1)), new java.util.HashMap<>(java.util.Map.of("cat", "b", "val", 5)), new java.util.HashMap<>(java.util.Map.of("cat", "b", "val", 2))};
      2| Object[] grouped = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(items);
      5| java.util.List<_Group> _grps = _group_by(_src, i -> i.get("cat"));

- group_items_iteration: line 34: unsupported line
     32| }
     33| 
>>>  34| static <T> T _cast(Class<T> cls, Object v) {
     35| if (cls.isInstance(v)) return cls.cast(v);
     36| if (cls == Integer.class) {

- if_else: line 2: unsupported line
      1| int x = 5;
>>>   2| if ((x > 3)) {
      3| System.out.println("big");
      4| } else {

- if_then_else: parse error: parse error: 3:22: lexer: invalid input text "? \"yes\" : \"no\"\n ..."
- if_then_else_nested: parse error: parse error: 3:21: lexer: invalid input text "? \"big\" : ((x > ..."
- in_operator: type error: error[T003]: unknown function: _in
  --> :4:10

help:
  Ensure the function is defined before it's called.
- in_operator_extended: line 3: unsupported line
      1| Object[] xs = new int[]{1, 2, 3};
      2| Object[] ys = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(xs);
      5| _src = _filter(_src, (Object x) -> { return ((x % 2) == 1); });

- inner_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1, "total", 250)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 2, "total", 125)), new java.util.HashMap<>(java.util.Map.of("id", 102, "customerId", 1, "total", 300)), new java.util.HashMap<>(java.util.Map.of("id", 103, "customerId", 4, "total", 80))};
      3| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(orders);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- join_multi: line 5: unsupported line
      3| Object[] items = new Object[]{new java.util.HashMap<>(java.util.Map.of("orderId", 100, "sku", "a")), new java.util.HashMap<>(java.util.Map.of("orderId", 101, "sku", "b"))};
      4| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   5| public java.util.List<Object> get() {
      6| java.util.List<Object> _src = _toList(orders);
      7| java.util.List<_JoinSpec> _joins = java.util.List.of(

- json_builtin: line 2: unsupported line
      1| java.util.Map<String, Integer> m = new java.util.HashMap<>(java.util.Map.of("a", 1, "b", 2));
>>>   2| _json(m);

- left_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1, "total", 250)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 3, "total", 80))};
      3| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(orders);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- left_join_multi: line 5: unsupported line
      3| Object[] items = new Object[]{new java.util.HashMap<>(java.util.Map.of("orderId", 100, "sku", "a"))};
      4| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   5| public java.util.List<Object> get() {
      6| java.util.List<Object> _src = _toList(orders);
      7| java.util.List<_JoinSpec> _joins = java.util.List.of(

- len_builtin: ok
- len_map: parse error: parse error: 2:27: unexpected token ">" (expected PostfixExpr)
- len_string: type error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- let_and_print: ok
- list_assign: line 8: unsupported line
      6| }
      7| 
>>>   8| static <T> T _cast(Class<T> cls, Object v) {
      9| if (cls.isInstance(v)) return cls.cast(v);
     10| if (cls == Integer.class) {

- list_index: ok
- list_nested_assign: line 8: unsupported line
      6| }
      7| 
>>>   8| static <T> T _cast(Class<T> cls, Object v) {
      9| if (cls.isInstance(v)) return cls.cast(v);
     10| if (cls == Integer.class) {

- list_set_ops: line 30: unsupported line
     28| }
     29| 
>>>  30| static <T> T[] _concat(T[] a, T[] b) {
     31| T[] res = java.util.Arrays.copyOf(a, a.length + b.length);
     32| System.arraycopy(b, 0, res, a.length, b.length);

- load_yaml: line 3: unsupported line
      1| Object[] people = _load("../interpreter/valid/people.yaml", new java.util.HashMap<>(java.util.Map.of("format", "yaml")));
      2| Object[] adults = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(people);
      5| _src = _filter(_src, (Object p) -> { return (p.get("age") >= 18); });

- map_assign: line 2: unsupported line
      1| java.util.Map<String, Integer> scores = new java.util.HashMap<>(java.util.Map.of("alice", 1));
>>>   2| scores.put("bob", 2);
      3| System.out.println(scores.get("bob"));

- map_in_operator: ok
- map_index: ok
- map_int_key: ok
- map_literal_dynamic: parse error: parse error: 5:14: unexpected token ")" (expected "]")
- map_membership: ok
- map_nested_assign: line 8: unsupported line
      6| }
      7| 
>>>   8| static <T> T _cast(Class<T> cls, Object v) {
      9| if (cls.isInstance(v)) return cls.cast(v);
     10| if (cls == Integer.class) {

- match_expr: parse error: parse error: 3:94: lexer: invalid input text "; if (java.util...."
- match_full: line 2: unsupported line
      1| return (new java.util.function.Supplier<String>() {
>>>   2| public String get() {
      3| Object _t = n;
      4| if (java.util.Objects.equals(_t, 0)) return "zero";

- math_ops: ok
- membership: ok
- min_max_builtin: ok
- nested_function: line 14: unsupported line
     12| }
     13| 
>>>  14| static <T> T _cast(Class<T> cls, Object v) {
     15| if (cls.isInstance(v)) return cls.cast(v);
     16| if (cls == Integer.class) {

- order_by_map: line 3: unsupported line
      1| Object[] data = new Object[]{new java.util.HashMap<>(java.util.Map.of("a", 1, "b", 2)), new java.util.HashMap<>(java.util.Map.of("a", 1, "b", 1)), new java.util.HashMap<>(java.util.Map.of("a", 0, "b", 5))};
      2| Object[] sorted = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(data);
      5| java.util.List<_JoinSpec> _joins = java.util.List.of(

- outer_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1, "total", 250)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 2, "total", 125)), new java.util.HashMap<>(java.util.Map.of("id", 102, "customerId", 1, "total", 300)), new java.util.HashMap<>(java.util.Map.of("id", 103, "customerId", 5, "total", 80))};
      3| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(orders);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- partial_application: ok
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type error: error[T002]: undefined variable: k
  --> :2:14

help:
  Check if the variable was declared in this scope.
- query_sum_select: line 3: unsupported line
      1| Object[] nums = new int[]{1, 2, 3};
      2| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(nums);
      5| _src = _filter(_src, (Object n) -> { return (n > 1); });

- record_assign: line 22: unsupported line
     20| }
     21| 
>>>  22| static <T> T _cast(Class<T> cls, Object v) {
     23| if (cls.isInstance(v)) return cls.cast(v);
     24| if (cls == Integer.class) {

- right_join: line 4: unsupported line
      2| Object[] orders = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "customerId", 1, "total", 250)), new java.util.HashMap<>(java.util.Map.of("id", 101, "customerId", 2, "total", 125)), new java.util.HashMap<>(java.util.Map.of("id", 102, "customerId", 1, "total", 300))};
      3| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   4| public java.util.List<Object> get() {
      5| java.util.List<Object> _src = _toList(customers);
      6| java.util.List<_JoinSpec> _joins = java.util.List.of(

- save_jsonl_stdout: line 2: unsupported line
      1| Object[] people = new Object[]{new java.util.HashMap<>(java.util.Map.of("name", "Alice", "age", 30)), new java.util.HashMap<>(java.util.Map.of("name", "Bob", "age", 25))};
>>>   2| _save(people, "-", new java.util.HashMap<>(java.util.Map.of("format", "jsonl")));

- short_circuit: ok
- slice: line 41: unsupported line
     39| }
     40| 
>>>  41| static <T> T[] _slice(T[] arr, int i, int j) {
     42| if (i < 0) i += arr.length;
     43| if (j < 0) j += arr.length;

- sort_stable: line 3: unsupported line
      1| Object[] items = new Object[]{new java.util.HashMap<>(java.util.Map.of("n", 1, "v", "a")), new java.util.HashMap<>(java.util.Map.of("n", 1, "v", "b")), new java.util.HashMap<>(java.util.Map.of("n", 2, "v", "c"))};
      2| Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(items);
      5| java.util.List<_JoinSpec> _joins = java.util.List.of(

- str_builtin: ok
- string_compare: ok
- string_concat: ok
- string_contains: parse error: parse error: 3:21: unexpected token ")" (expected "]")
- string_in_operator: ok
- string_index: ok
- string_prefix_slice: type error: error[T003]: unknown function: _sliceString
  --> :4:9

help:
  Ensure the function is defined before it's called.
- substring_builtin: ok
- sum_builtin: parse error: parse error: 2:18: unexpected token "int" (expected ")")
- tail_recursion: line 3: unsupported line
      1| if ((n == 0)) {
      2| return acc;
>>>   3| }
      4| return sum_rec((n - 1), (acc + n));

- test_block: line 1: unsupported line
>>>   1| test_addition_works();
      2| System.out.println("ok");

- tree_sum: line 24: unsupported line
     22| }
     23| 
>>>  24| static <T> T _cast(Class<T> cls, Object v) {
     25| if (cls.isInstance(v)) return cls.cast(v);
     26| if (cls == Integer.class) {

- two-sum: line 3: unsupported line
      1| if (((nums[i] + nums[j]) == target)) {
      2| return new int[] {i, j};
>>>   3| }

- typed_let: line 1: unsupported line
>>>   1| int y;
      2| System.out.println(y);

- typed_var: line 1: unsupported line
>>>   1| int x;
      2| System.out.println(x);

- unary_neg: ok
- update_stmt: line 27: unsupported line
     25| }
     26| 
>>>  27| static Object[] people =
     28| new Person[] {
     29| new Person("Alice", 17, "minor"),

- user_type_literal: ok
- values_builtin: ok
- var_assignment: ok
- while_loop: ok
