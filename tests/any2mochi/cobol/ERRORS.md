# Errors

- append_builtin: type2 error: error[T001]: assignment to undeclared variable: a
  --> :2:3

help:
  Declare `a` first using `let`.
- avg_builtin: parse2 error: parse error: 11:11: unexpected token "=" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- basic_compare: convert error: syntax error at line 16: syntax error, unexpected =
  15 |     DISPLAY A
  16 |     COMPUTE TMP0 = A = 7
  17 |     DISPLAY TMP0
- binary_precedence: output mismatch
- bool_chain: convert error: syntax error at line 13: syntax error, unexpected <, expecting ) or - or +
  12 | PROCEDURE DIVISION.
  13 |     COMPUTE TMP0 = (1 < 2) * (2 < 3) * (3 < 4)
  14 |     DISPLAY TMP0
- break_continue: convert error: syntax error at line 7: syntax error, unexpected NUMBERS
   6 | WORKING-STORAGE SECTION.
   7 | 01 NUMBERS OCCURS 9 TIMES PIC 9.
   8 | 01 IDX PIC 9.
- cast_string_to_int: output mismatch
- cast_struct: convert error: syntax error at line 11: 'TODO_TITLE' is not defined
  10 |     COMPUTE TODO = 0
  11 |     DISPLAY TODO_TITLE
  12 |     STOP RUN.
- closure: convert error: syntax error at line 23: 'LAMBDA0' is not defined
  22 | *> function MAKEADDER
  23 | FMAKEADDER.
  24 |     COMPUTE MAKEADDER_RES = LAMBDA0
- count_builtin: output mismatch
- cross_join: convert error: unsupported feature at line 42:  unsupported for-in loop
  41 |     DISPLAY "--- Cross Join: All order-customer pairs ---"
  42 |     *> unsupported for-in loop
  43 |     STOP RUN.
- cross_join_filter: convert error: syntax error at line 30: PERFORM statement not terminated by END-PERFORM
  29 |             MOVE 0 TO IDX2
  30 |             PERFORM VARYING IDX2 FROM 0 BY 1 UNTIL IDX2 >= 2
  31 |             MOVE LETTERS(IDX2 + 1) TO L
- cross_join_triple: convert error: unsupported feature at line 49:  unsupported for-in loop
  48 |     DISPLAY "--- Cross Join of three lists ---"
  49 |     *> unsupported for-in loop
  50 |     STOP RUN.
- dataset_sort_take_limit: convert error: unsupported feature at line 36:  unsupported for-in loop
  35 |     DISPLAY "--- Top products (excluding most expensive) ---"
  36 |     *> unsupported for-in loop
  37 |     STOP RUN.
- dataset_where_filter: convert error: syntax error at line 24: 'PERSON_AGE' is not defined
  23 |     MOVE PEOPLE(IDX + 1) TO PERSON
  24 |     IF PERSON_AGE >= 18
  25 |         ADD 1 TO TMP1
- exists_builtin: convert error: syntax error at line 7: syntax error, unexpected DATA
   6 | WORKING-STORAGE SECTION.
   7 | 01 DATA OCCURS 2 TIMES PIC 9.
   8 | 01 FLAG PIC 9.
- for_list_collection: type2 error: error[T001]: assignment to undeclared variable: tmp0
  --> :2:3

help:
  Declare `tmp0` first using `let`.
- for_loop: output mismatch
- for_map_collection: convert error: unsupported feature at line 11:  unsupported for-in loop
  10 |     COMPUTE M = 0
  11 |     *> unsupported for-in loop
  12 |     STOP RUN.
- fun_call: output mismatch
- fun_expr_in_let: type2 error: error[T003]: unknown function: flambda0
  --> :3:3

help:
  Ensure the function is defined before it's called.
- fun_three_args: type2 error: error[T003]: unknown function: fsum3
  --> :5:3

help:
  Ensure the function is defined before it's called.
- group_by: convert error: unsupported feature at line 35:  unsupported for-in loop
  34 |     DISPLAY "--- People grouped by city ---"
  35 |     *> unsupported for-in loop
  36 |     STOP RUN.
- group_by_conditional_sum: convert error: syntax error at line 31: 'RESULT' requires one subscript
  30 |     END-PERFORM
  31 |     DISPLAY RESULT
  32 |     STOP RUN.
- group_by_having: parse2 error: parse error: 16:12: unexpected token "=" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- group_by_join: convert error: syntax error at line 31: 'O_CUSTOMERID' is not defined
  30 |         MOVE CUSTOMERS(IDX2 + 1) TO C
  31 |         IF O_CUSTOMERID = C_ID
  32 |             ADD 1 TO TMP1
- group_by_left_join: convert error: syntax error at line 32: 'O_CUSTOMERID' is not defined
  31 |         MOVE ORDERS(IDX2 + 1) TO O
  32 |         IF O_CUSTOMERID = C_ID
  33 |             ADD 1 TO TMP1
- group_by_multi_join: convert error: syntax error at line 43: 'N_NAME' is not defined
  42 |             MOVE NATIONS(IDX3 + 1) TO N
  43 |             IF N_NAME = "A" * S_ID = PS_SUPPLIER * N_ID = S_NATION
  44 |                 ADD 1 TO TMP1
- group_by_multi_join_sort: convert error: syntax error at line 32: 'literal "1993-10-01"' is not a numeric value
  31 |     MOVE 0 TO LINEITEM(2)
  32 |     COMPUTE START_DATE = "1993-10-01"
  33 |     COMPUTE END_DATE = "1994-01-01"
- group_by_sort: convert error: syntax error at line 32: 'GROUPED' requires one subscript
  31 |     END-PERFORM
  32 |     DISPLAY GROUPED
  33 |     STOP RUN.
- group_items_iteration: convert error: syntax error at line 7: syntax error, unexpected DATA
   6 | WORKING-STORAGE SECTION.
   7 | 01 DATA OCCURS 3 TIMES PIC 9.
   8 | 01 D PIC 9.
- if_else: output mismatch
- if_then_else: convert error: syntax error at line 18: 'TMP0' is not a numeric value
  17 |     END-IF
  18 |     COMPUTE MSG = TMP0
  19 |     DISPLAY MSG
- if_then_else_nested: convert error: syntax error at line 24: 'TMP0' is not a numeric value
  23 |     END-IF
  24 |     COMPUTE MSG = TMP0
  25 |     DISPLAY MSG
- in_operator: convert error: syntax error at line 15: syntax error, unexpected IN
  14 |     MOVE 3 TO XS(3)
  15 |     COMPUTE TMP0 = 2 in XS
  16 |     DISPLAY TMP0
- in_operator_extended: convert error: syntax error at line 41: syntax error, unexpected IN
  40 |     END-PERFORM
  41 |     COMPUTE TMP2 = 1 in YS
  42 |     DISPLAY TMP2
- inner_join: convert error: syntax error at line 33: 'O_CUSTOMERID' is not defined
  32 |         MOVE CUSTOMERS(IDX2 + 1) TO C
  33 |         IF O_CUSTOMERID = C_ID
  34 |             ADD 1 TO TMP1
- join_multi: convert error: syntax error at line 38: 'O_CUSTOMERID' is not defined
  37 |             MOVE ITEMS(IDX3 + 1) TO I
  38 |             IF O_CUSTOMERID = C_ID * O_ID = I_ORDERID
  39 |                 ADD 1 TO TMP1
- json_builtin: output mismatch
- left_join: convert error: syntax error at line 30: 'O_CUSTOMERID' is not defined
  29 |         MOVE CUSTOMERS(IDX2 + 1) TO C
  30 |         IF O_CUSTOMERID = C_ID
  31 |             ADD 1 TO TMP1
- left_join_multi: convert error: syntax error at line 37: 'O_CUSTOMERID' is not defined
  36 |             MOVE ITEMS(IDX3 + 1) TO I
  37 |             IF O_CUSTOMERID = C_ID * O_ID = I_ORDERID
  38 |                 ADD 1 TO TMP1
- len_builtin: output mismatch
- len_map: convert error: syntax error at line 10: a non-numeric literal is expected here
   9 | PROCEDURE DIVISION.
  10 |     COMPUTE TMP0 = FUNCTION LENGTH(0)
  11 |     DISPLAY TMP0
- len_string: type2 error: error[T002]: undefined variable: FUNCTION
  --> :2:14

help:
  Check if the variable was declared in this scope.
- let_and_print: type2 error: error[T002]: undefined variable: b
  --> :3:18

help:
  Check if the variable was declared in this scope.
- list_assign: compile panic: interface conversion: interface {} is nil, not string
- list_index: type2 error: error[T001]: assignment to undeclared variable: xs
  --> :2:3

help:
  Declare `xs` first using `let`.
- list_nested_assign: compile panic: interface conversion: interface {} is nil, not string
- list_set_ops: convert error: syntax error at line 13: syntax error, unexpected Identifier
  12 | PROCEDURE DIVISION.
  13 |     COMPUTE TMP0 = 0 union 0
  14 |     DISPLAY TMP0
- load_yaml: convert error: syntax error at line 21: 'P_AGE' is not defined
  20 |     MOVE PEOPLE(IDX + 1) TO P
  21 |     IF P_AGE >= 18
  22 |         ADD 1 TO TMP1
- map_assign: compile panic: interface conversion: interface {} is nil, not string
- map_in_operator: convert error: syntax error at line 13: syntax error, unexpected IN
  12 |     COMPUTE M = 0
  13 |     COMPUTE TMP0 = 1 in M
  14 |     DISPLAY TMP0
- map_index: convert error: syntax error at line 12: 'literal "b"' is not a numeric value
  11 |     COMPUTE M = 0
  12 |     COMPUTE TMP0 = M("b" + 1)
  13 |     DISPLAY TMP0
- map_int_key: convert error: syntax error at line 12: 'M' cannot be subscripted
  11 |     COMPUTE M = 0
  12 |     COMPUTE TMP0 = M(1 + 1)
  13 |     DISPLAY TMP0
- map_literal_dynamic: output mismatch
- map_membership: convert error: syntax error at line 13: 'literal "a"' is not a numeric value
  12 |     COMPUTE M = 0
  13 |     COMPUTE TMP0 = "a" in M
  14 |     DISPLAY TMP0
- map_nested_assign: compile panic: interface conversion: interface {} is nil, not string
- match_expr: convert error: syntax error at line 8: syntax error, unexpected LABEL
   7 | 01 X PIC 9.
   8 | 01 LABEL PIC X(100).
   9 | 01 TMP0 PIC 9.
- match_full: convert error: syntax error at line 8: syntax error, unexpected LABEL
   7 | 01 X PIC 9.
   8 | 01 LABEL PIC X(100).
   9 | 01 TMP0 PIC 9.
- math_ops: type2 error: error[T002]: undefined variable: FUNCTION
  --> :6:14

help:
  Check if the variable was declared in this scope.
- membership: convert error: syntax error at line 15: syntax error, unexpected IN
  14 |     MOVE 3 TO NUMS(3)
  15 |     COMPUTE TMP0 = 2 in NUMS
  16 |     DISPLAY TMP0
- min_max_builtin: type2 error: error[T001]: assignment to undeclared variable: nums
  --> :2:3

help:
  Declare `nums` first using `let`.
- nested_function: convert error: syntax error at line 19: 'X' cannot be used here
  18 | *> function INNER
  19 | FINNER.
  20 |     COMPUTE INNER_RES = X + INNER_P0
- order_by_map: convert error: syntax error at line 7: syntax error, unexpected DATA
   6 | WORKING-STORAGE SECTION.
   7 | 01 DATA OCCURS 3 TIMES PIC 9.
   8 | 01 X PIC 9.
- outer_join: convert error: syntax error at line 34: 'O_CUSTOMERID' is not defined
  33 |         MOVE CUSTOMERS(IDX2 + 1) TO C
  34 |         IF O_CUSTOMERID = C_ID
  35 |             ADD 1 TO TMP1
- partial_application: convert error: syntax error at line 10: 'ADD_P0' is not defined
   9 | PROCEDURE DIVISION.
  10 |     COMPUTE ADD_P0 = 5
  11 |     PERFORM FADD
- print_hello: output mismatch
- pure_fold: type2 error: error[T003]: unknown function: ftriple
  --> :4:3

help:
  Ensure the function is defined before it's called.
- pure_global_fold: type2 error: error[T003]: unknown function: finc
  --> :4:3

help:
  Ensure the function is defined before it's called.
- query_sum_select: convert error: syntax error at line 36: 'RESULT' requires one subscript
  35 |     END-PERFORM
  36 |     DISPLAY RESULT
  37 |     STOP RUN.
- record_assign: convert error: syntax error at line 13: 'C' cannot be used here
  12 |     COMPUTE C_N = 0
  13 |     COMPUTE INC_P0 = C
  14 |     PERFORM FINC
- right_join: convert error: syntax error at line 33: 'O_CUSTOMERID' is not defined
  32 |         MOVE ORDERS(IDX2 + 1) TO O
  33 |         IF O_CUSTOMERID = C_ID
  34 |             ADD 1 TO TMP1
- save_jsonl_stdout: type2 error: error[T001]: assignment to undeclared variable: people
  --> :2:3

help:
  Declare `people` first using `let`.
- short_circuit: type2 error: error[T003]: unknown function: fboom
  --> :4:3

help:
  Ensure the function is defined before it's called.
- slice: convert error: syntax error at line 19: 'TMP0' requires one subscript
  18 |     MOVE 3 TO TMP0(2)
  19 |     COMPUTE TMP1 = TMP0
  20 |     DISPLAY TMP1
- sort_stable: convert error: syntax error at line 27: 'I_V' is not defined
  26 |             ADD 1 TO TMP0
  27 |             COMPUTE RESULT(TMP0) = I_V
  28 |         END-IF
- str_builtin: output mismatch
- string_compare: convert error: syntax error at line 13: 'literal "a"' is not a numeric value
  12 | PROCEDURE DIVISION.
  13 |     COMPUTE TMP0 = "a" < "b"
  14 |     DISPLAY TMP0
- string_concat: parse2 error: parse error: 2:23: lexer: invalid input text "& \"world\"\n  prin..."
- string_contains: convert error: syntax error at line 10: 'literal "catch"' is not a numeric value
   9 | PROCEDURE DIVISION.
  10 |     COMPUTE S = "catch"
  11 |     DISPLAY 0
- string_in_operator: convert error: syntax error at line 12: 'literal "catch"' is not a numeric value
  11 | PROCEDURE DIVISION.
  12 |     COMPUTE S = "catch"
  13 |     COMPUTE TMP0 = "cat" in S
- string_index: convert error: syntax error at line 15: 'literal "mochi"' is not a numeric value
  14 | PROCEDURE DIVISION.
  15 |     COMPUTE S = "mochi"
  16 |     MOVE 1 TO TMP0
- string_prefix_slice: convert error: syntax error at line 22: 'literal "fore"' is not a numeric value
  21 | PROCEDURE DIVISION.
  22 |     COMPUTE PREFIX = "fore"
  23 |     COMPUTE S1 = "forest"
- substring_builtin: output mismatch
- sum_builtin: type2 error: error[T001]: assignment to undeclared variable: tmp0
  --> :2:3

help:
  Declare `tmp0` first using `let`.
- tail_recursion: convert error: syntax error at line 22: syntax error, unexpected END-IF
  21 |         COMPUTE SUM_REC_RES = SUM_REC_P1
  22 |         EXIT.
  23 |         END-IF
- test_block: parse2 error: parse error: 4:13: unexpected token "=" (expected ")")
- tree_sum: convert error: syntax error at line 18: 'LEAF' is not defined
  17 | PROCEDURE DIVISION.
  18 |     COMPUTE T_LEFT = LEAF
  19 |     COMPUTE T_VALUE = 1
- two-sum: parse2 error: parse error: 14:36: unexpected token "=" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- typed_let: output mismatch
- typed_var: output mismatch
- unary_neg: parse2 error: parse error: 3:18: unexpected token "-" (expected PostfixExpr)
- update_stmt: convert error: unsupported feature at line 10:  unsupported update statement
   9 | PROCEDURE DIVISION.
  10 |     *> unsupported update statement
  11 | DISPLAY "-- TEST update adult status --"
- user_type_literal: output mismatch
- values_builtin: output mismatch
- var_assignment: output mismatch
- while_loop: output mismatch
