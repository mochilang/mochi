# Errors

- break_continue: line 2: unsupported line
      1| Object[] numbers = new int[] {1, 2, 3, 4, 5, 6, 7, 8, 9};
>>>   2| for (var n : numbers) {
      3| if (((n % 2) == 0)) {
      4| continue;

- cast_struct: line 17: unsupported line
     15| }
     16| 
>>>  17| static <T> T _cast(Class<T> cls, Object v) {
     18| if (cls.isInstance(v)) return cls.cast(v);
     19| if (cls == Integer.class) {

- dataset: line 3: unsupported line
      1| Object[] people = new Object[]{new java.util.HashMap<>(java.util.Map.of("name", "Alice", "age", 30)), new java.util.HashMap<>(java.util.Map.of("name", "Bob", "age", 15)), new java.util.HashMap<>(java.util.Map.of("name", "Charlie", "age", 65)), new java.util.HashMap<>(java.util.Map.of("name", "Diana", "age", 45))};
      2| Object[] adults = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(people);
      5| _src = _filter(_src, (Object person) -> { return (person.age >= 18); });

- dataset_sort_take_limit: line 3: unsupported line
      1| Object[] products = new Product[]{new Product("Laptop", 1500), new Product("Smartphone", 900), new Product("Tablet", 600), new Product("Monitor", 300), new Product("Keyboard", 100), new Product("Mouse", 50), new Product("Headphones", 200)};
      2| Object[] expensive = (new java.util.function.Supplier<java.util.List<Object>>() {
>>>   3| public java.util.List<Object> get() {
      4| java.util.List<Object> _src = _toList(products);
      5| java.util.List<_JoinSpec> _joins = java.util.List.of(

- factorial: line 3: unsupported line
      1| if ((n <= 1)) {
      2| return 1;
>>>   3| }
      4| return (n * factorial((n - 1)));

- fetch_builtin: line 17: unsupported line
     15| }
     16| 
>>>  17| static <T> T _cast(Class<T> cls, Object v) {
     18| if (cls.isInstance(v)) return cls.cast(v);
     19| if (cls == Integer.class) {

- fetch_http: line 23: unsupported line
     21| }
     22| 
>>>  23| static <T> T _cast(Class<T> cls, Object v) {
     24| if (cls.isInstance(v)) return cls.cast(v);
     25| if (cls == Integer.class) {

- fibonacci: line 3: unsupported line
      1| if ((n <= 1)) {
      2| return n;
>>>   3| }
      4| return (fib((n - 1)) + fib((n - 2)));

- for_list_collection: line 1: unsupported line
>>>   1| for (var n : new int[] {1, 2, 3}) {
      2| System.out.println(n);

- for_loop: line 3: unsupported line
      1| for (int i = 1; i < 4; i++) {
      2| System.out.println(i);
>>>   3| }

- for_string_collection: line 1: unsupported line
>>>   1| for (var ch : "hi".toCharArray()) {
      2| System.out.println(ch);
      3| }

- fun_expr_in_let: type error: error[T003]: unknown function: fn
  --> :2:16

help:
  Ensure the function is defined before it's called.
- input_builtin: parse error: parse error: 1:41: unexpected token "in" (expected <ident>)
- load_save_json: line 305: unsupported line
    303| if (!m) { java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(null); joined.add(row); }
    304| }
>>> 305| for (int ri=0; ri<jitems.size(); ri++) {
    306| if (!matched[ri]) { java.util.List<Object> undef = new java.util.ArrayList<>(items.isEmpty()?0:items.get(0).size()); for(int k=0;k<undef.size();k++) undef.set(k,null); undef.add(jitems.get(ri)); joined.add(undef); }
    307| }

- loop_string: line 2: unsupported line
      1| int c = 0;
>>>   2| for (var ch : "ab".toCharArray()) {
      3| c = (c + 1);
      4| }

- map_len: parse error: parse error: 2:27: unexpected token ">" (expected PostfixExpr)
- map_set: line 2: unsupported line
      1| java.util.Map<String, Integer> scores = new java.util.HashMap<>(java.util.Map.of("a", 1));
>>>   2| scores.put("b", 2);
      3| System.out.println(scores.get("b"));

- matrix_search: line 4: unsupported line
      2| if ((m == 0)) {
      3| return false;
>>>   4| }
      5| int n = matrix[0].length;
      6| int left = 0;

- nested_fn: line 14: unsupported line
     12| }
     13| 
>>>  14| static <T> T _cast(Class<T> cls, Object v) {
     15| if (cls.isInstance(v)) return cls.cast(v);
     16| if (cls == Integer.class) {

- package_decl: line 1: unsupported line
>>>   1| package sample;
      2| 
      3| public class Main {

- test_block: line 1: unsupported line
>>>   1| test_addition_works();
      2| System.out.println("ok");

- tpch_q1: line 8: unsupported line
      6| static Object[] lineitem = new Object[]{new java.util.HashMap<>(java.util.Map.of("l_quantity", 17, "l_extendedprice", 1000, "l_discount", 0.05, "l_tax", 0.07, "l_returnflag", "N", "l_linestatus", "O", "l_shipdate", "1998-08-01")), new java.util.HashMap<>(java.util.Map.of("l_quantity", 36, "l_extendedprice", 2000, "l_discount", 0.1, "l_tax", 0.05, "l_returnflag", "N", "l_linestatus", "O", "l_shipdate", "1998-09-01")), new java.util.HashMap<>(java.util.Map.of("l_quantity", 25, "l_extendedprice", 1500, "l_discount", 0, "l_tax", 0.08, "l_returnflag", "R", "l_linestatus", "F", "l_shipdate", "1998-09-03"))};
      7| 
>>>   8| static Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
      9| public java.util.List<Object> get() {
     10| java.util.List<Object> _src = _toList(lineitem);

- two_sum: line 6: unsupported line
      4| if (((nums[i] + nums[j]) == target)) {
      5| return new int[] {i, j};
>>>   6| }
      7| }
      8| }

- underscore_for_loop: line 10: unsupported line
      8| c = (c + 1);
      9| }
>>>  10| for (var __ : "ab".toCharArray()) {
     11| c = (c + 1);
     12| }

- update_stmt: line 27: unsupported line
     25| }
     26| 
>>>  27| static Object[] people =
     28| new Person[] {
     29| new Person("Alice", 17, "minor"),

- while_loop: line 2: unsupported line
      1| int i = 0;
>>>   2| while ((i < 3)) {
      3| System.out.println(i);
      4| i = (i + 1);

- while_membership: line 9: unsupported line
      7| int i = 1;
      8| int count = 0;
>>>   9| while (_in(i, set)) {
     10| i = (i + 1);
     11| count = (count + 1);

