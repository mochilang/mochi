# OCaml VM roundtrip test failures

## tests/vm/valid/append_builtin.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3604641129/prog.ml", line 2, characters 30-36:
2 | print_endline (string_of_int (append a 3));;
                                  ^^^^^^
Error: Unbound value append

```

## tests/vm/valid/basic_compare.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3666755009/prog.ml", line 4, characters 29-36:
4 | print_endline (string_of_int (a = 7));;
                                 ^^^^^^^
Error: This expression has type bool but an expression was expected of type
         int

```

## tests/vm/valid/bool_chain.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1902709348/prog.ml", line 6, characters 21-22:
6 |   with Return_0 v -> v
                         ^
Error: This expression has type bool
       This is not a function; it cannot be applied.

```

## tests/vm/valid/cast_string_to_int.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2862457879/prog.ml", line 1, characters 16-22:
1 | print_endline (("1995" : int));;
                    ^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/cast_struct.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog559688582/prog.ml", line 1, characters 10-11:
1 | type Todo = {
              ^
Error: Syntax error

```

## tests/vm/valid/closure.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1186120676/prog.ml", line 4, characters 10-37:
4 |     raise (Return_0 (fun x -> x + n))
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The constructor Return_0 expects 0 argument(s),
       but is applied here to 1 argument(s)

```

## tests/vm/valid/cross_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1317104393/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 3;Hashtbl.add tbl "name" "Charlie"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/cross_join_filter.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1396334712/prog.ml", line 6, characters 55-56:
6 |     print_endline (String.concat " " [string_of_int (p.n); string_of_int (p.l)]);
                                                           ^
Error: Unbound record field n

```

## tests/vm/valid/cross_join_triple.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3255249995/prog.ml", line 7, characters 55-56:
7 |     print_endline (String.concat " " [string_of_int (c.n); string_of_int (c.l); string_of_int (c.b)]);
                                                           ^
Error: Unbound record field n

```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog663919/prog.ml", line 1, characters 103-107:
1 | let products = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Laptop";Hashtbl.add tbl "price" 1500; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Smartphone";Hashtbl.add tbl "price" 900; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Tablet";Hashtbl.add tbl "price" 600; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Monitor";Hashtbl.add tbl "price" 300; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Keyboard";Hashtbl.add tbl "price" 100; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Mouse";Hashtbl.add tbl "price" 50; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Headphones";Hashtbl.add tbl "price" 200; tbl)];;
                                                                                                           ^^^^
Error: This expression has type int but an expression was expected of type
         string

```

## tests/vm/valid/dataset_where_filter.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/exists_builtin.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2465013426/prog.ml", line 2, characters 11-17:
2 | let flag = exists [];;
               ^^^^^^
Error: Unbound value exists
Hint: Did you mean exit?

```

## tests/vm/valid/for_loop.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog812777905/prog.ml", line 2, characters 35-37:
2 |   print_endline (string_of_int (i));;
                                       ^^
Error: Syntax error

```

## tests/vm/valid/for_map_collection.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2679553566/prog.ml", line 4, characters 4-6:
4 |   ) !m;;
        ^^
Error: This expression has type (string, int) Hashtbl.t
       but an expression was expected of type int list

```

## tests/vm/valid/fun_call.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2105312821/prog.ml", line 5, characters 21-22:
5 |   with Return_0 v -> v
                         ^
Error: This expression has type int
       This is not a function; it cannot be applied.

```

## tests/vm/valid/fun_three_args.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3465610871/prog.ml", line 5, characters 21-22:
5 |   with Return_0 v -> v
                         ^
Error: This expression has type int
       This is not a function; it cannot be applied.

```

## tests/vm/valid/group_by.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1342130861/prog.ml", line 1, characters 98-100:
1 | let people = [(let tbl = Hashtbl.create 3 in Hashtbl.add tbl "name" "Alice";Hashtbl.add tbl "age" 30;Hashtbl.add tbl "city" "Paris"; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "name" "Bob";Hashtbl.add tbl "age" 15;Hashtbl.add tbl "city" "Hanoi"; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "name" "Charlie";Hashtbl.add tbl "age" 65;Hashtbl.add tbl "city" "Paris"; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "name" "Diana";Hashtbl.add tbl "age" 45;Hashtbl.add tbl "city" "Hanoi"; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "name" "Eve";Hashtbl.add tbl "age" 70;Hashtbl.add tbl "city" "Paris"; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "name" "Frank";Hashtbl.add tbl "age" 22;Hashtbl.add tbl "city" "Hanoi"; tbl)];;
                                                                                                      ^^
Error: This expression has type int but an expression was expected of type
         string

```

## tests/vm/valid/group_by_conditional_sum.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2302373964/prog.ml", line 1, characters 92-94:
1 | let items = [(let tbl = Hashtbl.create 3 in Hashtbl.add tbl "cat" "a";Hashtbl.add tbl "val" 10;Hashtbl.add tbl "flag" true; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "cat" "a";Hashtbl.add tbl "val" 5;Hashtbl.add tbl "flag" false; tbl); (let tbl = Hashtbl.create 3 in Hashtbl.add tbl "cat" "b";Hashtbl.add tbl "val" 20;Hashtbl.add tbl "flag" true; tbl)];;
                                                                                                ^^
Error: This expression has type int but an expression was expected of type
         string

```

## tests/vm/valid/group_by_having.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1671393486/prog.ml", line 3, characters 0-4:
3 | json big;;
    ^^^^
Error: Unbound value json

```

## tests/vm/valid/group_by_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3342618601/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/group_by_left_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog236923406/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 3;Hashtbl.add tbl "name" "Charlie"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/group_by_multi_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1141164182/prog.ml", line 1, characters 92-95:
1 | let nations = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "A"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "B"; tbl)];;
                                                                                                ^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1416433827/prog.ml", line 1, characters 102-110:
1 | let nation = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "n_nationkey" 1;Hashtbl.add tbl "n_name" "BRAZIL"; tbl)];;
                                                                                                          ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/group_by_sort.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2822702405/prog.ml", line 1, characters 92-93:
1 | let items = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "cat" "a";Hashtbl.add tbl "val" 3; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "cat" "a";Hashtbl.add tbl "val" 1; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "cat" "b";Hashtbl.add tbl "val" 5; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "cat" "b";Hashtbl.add tbl "val" 2; tbl)];;
                                                                                                ^
Error: This expression has type int but an expression was expected of type
         string

```

## tests/vm/valid/group_items_iteration.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1331982766/prog.ml", line 7, characters 28-31:
7 |         total := !total + x.val;
                                ^^^
Error: Syntax error

```

## tests/vm/valid/if_else.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3463356000/prog.ml", line 3, characters 21-23:
3 |   print_endline "big";;
                         ^^
Error: Syntax error: 'end' expected
File "/tmp/ocamlprog3463356000/prog.ml", line 2, characters 14-19:
2 | if x > 3 then begin
                  ^^^^^
  This 'begin' might be unmatched

```

## tests/vm/valid/if_then_else.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/if_then_else_nested.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/in_operator.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2023247919/prog.ml", line 2, characters 42-44:
2 | print_endline (string_of_int (Hashtbl.mem xs 2));;
                                              ^^
Error: This expression has type int list
       but an expression was expected of type ('a, 'b) Hashtbl.t

```

## tests/vm/valid/in_operator_extended.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2990222498/prog.ml", line 3, characters 42-44:
3 | print_endline (string_of_int (Hashtbl.mem ys 1));;
                                              ^^
Error: This expression has type 'a list
       but an expression was expected of type ('b, 'c) Hashtbl.t

```

## tests/vm/valid/inner_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog442086867/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 3;Hashtbl.add tbl "name" "Charlie"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/join_multi.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1145579722/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/json_builtin.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3096948292/prog.ml", line 2, characters 0-4:
2 | json m;;
    ^^^^
Error: Unbound value json

```

## tests/vm/valid/left_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2872078026/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/left_join_multi.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3560305338/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/list_nested_assign.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3713863202/prog.ml", line 8, characters 29-32:
8 | matrix := _set_nth !matrix 1 (5);;
                                 ^^^
Error: This expression has type int but an expression was expected of type
         int list

```

## tests/vm/valid/list_set_ops.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog4280404504/prog.ml", line 5, characters 29-53:
5 | print_endline (string_of_int ((_union [1; 2] [2; 3])));;
                                 ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type int list
       but an expression was expected of type int

```

## tests/vm/valid/load_yaml.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2126920241/prog.ml", line 30, characters 12-13:
30 | type Person = {
                 ^
Error: Syntax error

```

## tests/vm/valid/map_in_operator.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1406788474/prog.ml", line 2, characters 29-46:
2 | print_endline (string_of_int (Hashtbl.mem m 1));;
                                 ^^^^^^^^^^^^^^^^^
Error: This expression has type bool but an expression was expected of type
         int

```

## tests/vm/valid/map_int_key.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3439286870/prog.ml", line 2, characters 29-49:
2 | print_endline (string_of_int ((Hashtbl.find m 1)));;
                                 ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/map_membership.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog519539548/prog.ml", line 2, characters 14-33:
2 | print_endline (Hashtbl.mem m "a");;
                  ^^^^^^^^^^^^^^^^^^^
Error: This expression has type bool but an expression was expected of type
         string

```

## tests/vm/valid/map_nested_assign.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog584620441/prog.ml", line 2, characters 30-31:
2 | Hashtbl.replace !data "outer" 2;;
                                  ^
Error: This expression has type int but an expression was expected of type
         (string, int) Hashtbl.t

```

## tests/vm/valid/match_full.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog4008375893/prog.ml", line 16, characters 29-41:
16 | print_endline (string_of_int (classify 0));;
                                  ^^^^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/math_ops.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3974743837/prog.ml", line 2, characters 29-67:
2 | print_endline (string_of_int (float_of_int (7) /. float_of_int (2)));;
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type float but an expression was expected of type
         int

```

## tests/vm/valid/membership.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2645347525/prog.ml", line 2, characters 42-46:
2 | print_endline (string_of_int (Hashtbl.mem nums 2));;
                                              ^^^^
Error: This expression has type int list
       but an expression was expected of type ('a, 'b) Hashtbl.t

```

## tests/vm/valid/nested_function.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2794896783/prog.ml", line 4, characters 0-9:
4 | exception Return_1 of int
    ^^^^^^^^^
Error: Syntax error

```

## tests/vm/valid/order_by_map.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog44292138/prog.ml", line 3, characters 29-37:
3 | print_endline (string_of_int (sorted));;
                                 ^^^^^^^^
Error: This expression has type 'a list
       but an expression was expected of type int

```

## tests/vm/valid/outer_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3773608709/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 3;Hashtbl.add tbl "name" "Charlie"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 4;Hashtbl.add tbl "name" "Diana"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/pure_fold.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1401427737/prog.ml", line 5, characters 21-22:
5 |   with Return_0 v -> v
                         ^
Error: This expression has type int
       This is not a function; it cannot be applied.

```

## tests/vm/valid/pure_global_fold.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog683984307/prog.ml", line 4, characters 25-26:
4 |     raise (Return_0 (x + k))
                             ^
Error: Unbound value k

```

## tests/vm/valid/query_sum_select.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog958396122/prog.ml", line 3, characters 31-39:
3 | print_endline (string_of_float (result));;
                                   ^^^^^^^^
Error: This expression has type 'a list
       but an expression was expected of type float

```

## tests/vm/valid/record_assign.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog4042423355/prog.ml", line 1, characters 13-14:
1 | type Counter = {
                 ^
Error: Syntax error

```

## tests/vm/valid/right_join.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog103855821/prog.ml", line 1, characters 94-101:
1 | let customers = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 1;Hashtbl.add tbl "name" "Alice"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 2;Hashtbl.add tbl "name" "Bob"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 3;Hashtbl.add tbl "name" "Charlie"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "id" 4;Hashtbl.add tbl "name" "Diana"; tbl)];;
                                                                                                  ^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/save_jsonl_stdout.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2822811834/prog.ml", line 30, characters 98-100:
30 | let people = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Alice";Hashtbl.add tbl "age" 30; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "name" "Bob";Hashtbl.add tbl "age" 25; tbl)];;
                                                                                                       ^^
Error: This expression has type int but an expression was expected of type
         string

```

## tests/vm/valid/short_circuit.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog284158102/prog.ml", line 6, characters 21-22:
6 |   with Return_0 v -> v
                         ^
Error: This expression has type bool
       This is not a function; it cannot be applied.

```

## tests/vm/valid/slice.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog277593956/prog.ml", line 7, characters 14-20:
7 |     else x :: _slice xs 0 (len - 1)
                  ^^^^^^
Error: This function has type 'a list -> int -> int -> 'a list
       It is applied to too many arguments; maybe you forgot a `;'.

```

## tests/vm/valid/sort_stable.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2752268016/prog.ml", line 1, characters 86-89:
1 | let items = [(let tbl = Hashtbl.create 2 in Hashtbl.add tbl "n" 1;Hashtbl.add tbl "v" "a"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "n" 1;Hashtbl.add tbl "v" "b"; tbl); (let tbl = Hashtbl.create 2 in Hashtbl.add tbl "n" 2;Hashtbl.add tbl "v" "c"; tbl)];;
                                                                                          ^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/str_builtin.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2188876504/prog.ml", line 1, characters 29-50:
1 | print_endline (string_of_int (string_of_int (123)));;
                                 ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int

```

## tests/vm/valid/string_compare.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog4249183021/prog.ml", line 1, characters 14-25:
1 | print_endline ("a" < "b");;
                  ^^^^^^^^^^^
Error: This expression has type bool but an expression was expected of type
         string

```

## tests/vm/valid/string_contains.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog217891480/prog.ml", line 2, characters 18-26:
2 | print_endline ((s.contains "cat"));;
                      ^^^^^^^^
Error: Unbound record field contains
Hint: Did you mean contents?

```

## tests/vm/valid/string_in_operator.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3883790105/prog.ml", line 2, characters 27-28:
2 | print_endline (Hashtbl.mem s "cat");;
                               ^
Error: This expression has type string but an expression was expected of type
         ('a, 'b) Hashtbl.t

```

## tests/vm/valid/string_index.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1309885490/prog.ml", line 2, characters 14-32:
2 | print_endline ((String.get s 1));;
                  ^^^^^^^^^^^^^^^^^^
Error: This expression has type char but an expression was expected of type
         string

```

## tests/vm/valid/string_prefix_slice.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1725812253/prog.ml", line 3, characters 14-69:
3 | print_endline ((String.sub s1 0 (String.length prefix - 0)) = prefix);;
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type bool but an expression was expected of type
         string

```

## tests/vm/valid/substring_builtin.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1282654454/prog.ml", line 1, characters 30-39:
1 | print_endline (string_of_int (substring "mochi" 1 4));;
                                  ^^^^^^^^^
Error: Unbound value substring

```

## tests/vm/valid/tail_recursion.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3938444155/prog.ml", line 7, characters 21-30:
7 |     raise (Return_0 (sum_rec n - 1 acc + n))
                         ^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
maybe some arguments are missing.
File "/tmp/ocamlprog3938444155/prog.ml", line 7, characters 21-30:
7 |     raise (Return_0 (sum_rec n - 1 acc + n))
                         ^^^^^^^^^
Error: This expression has type int -> 'a
       but an expression was expected of type int

```

## tests/vm/valid/tree_sum.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog264579308/prog.ml", line 4, characters 57-62:
4 |     raise (Return_0 ((match t with Leaf -> 0 | Node left value right -> sum_tree left + value + sum_tree right)))
                                                             ^^^^^
Error: Syntax error

```

## tests/vm/valid/typed_let.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog2851720015/prog.ml", line 2, characters 29-32:
2 | print_endline (string_of_int (y));;
                                 ^^^
Error: This expression has type unit but an expression was expected of type
         int

```

## tests/vm/valid/typed_var.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1197071898/prog.ml", line 2, characters 29-33:
2 | print_endline (string_of_int (!x));;
                                 ^^^^
Error: This expression has type unit but an expression was expected of type
         int

```

## tests/vm/valid/update_stmt.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog1960154418/prog.ml", line 1, characters 12-13:
1 | type Person = {
                ^
Error: Syntax error

```

## tests/vm/valid/user_type_literal.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog3078695572/prog.ml", line 1, characters 12-13:
1 | type Person = {
                ^
Error: Syntax error

```

## tests/vm/valid/values_builtin.mochi

```
ocamlc error: exit status 2
File "/tmp/ocamlprog403511333/prog.ml", line 2, characters 30-36:
2 | print_endline (string_of_int (values m));;
                                  ^^^^^^
Error: Unbound value values

```

