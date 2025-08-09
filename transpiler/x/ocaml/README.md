# Mochi OCaml Transpiler

This folder contains an experimental transpiler that converts Mochi source code into OCaml.

## Golden Test Checklist (102/105)

The list below tracks Mochi programs under `tests/vm/valid` that should successfully transpile. Checked items indicate tests known to work.

1. [x] append_builtin.mochi
2. [x] avg_builtin.mochi
3. [x] basic_compare.mochi
4. [x] bench_block.mochi
5. [x] binary_precedence.mochi
6. [x] bool_chain.mochi
7. [x] break_continue.mochi
8. [x] cast_string_to_int.mochi
9. [x] cast_struct.mochi
10. [x] closure.mochi
11. [x] count_builtin.mochi
12. [x] cross_join.mochi
13. [x] cross_join_filter.mochi
14. [x] cross_join_triple.mochi
15. [x] dataset_sort_take_limit.mochi
16. [x] dataset_where_filter.mochi
17. [x] exists_builtin.mochi
18. [x] for_list_collection.mochi
19. [x] for_loop.mochi
20. [x] for_map_collection.mochi
21. [x] fun_call.mochi
22. [x] fun_expr_in_let.mochi
23. [x] fun_three_args.mochi
24. [x] go_auto.mochi
25. [x] group_by.mochi
26. [x] group_by_conditional_sum.mochi
27. [x] group_by_having.mochi
28. [x] group_by_join.mochi
29. [x] group_by_left_join.mochi
30. [x] group_by_multi_join.mochi
31. [ ] group_by_multi_join_sort.mochi
32. [x] group_by_multi_sort.mochi
33. [x] group_by_sort.mochi
34. [x] group_items_iteration.mochi
35. [x] if_else.mochi
36. [x] if_then_else.mochi
37. [x] if_then_else_nested.mochi
38. [x] in_operator.mochi
39. [x] in_operator_extended.mochi
40. [x] inner_join.mochi
41. [x] join_multi.mochi
42. [x] json_builtin.mochi
43. [x] left_join.mochi
44. [x] left_join_multi.mochi
45. [x] len_builtin.mochi
46. [x] len_map.mochi
47. [x] len_string.mochi
48. [x] let_and_print.mochi
49. [x] list_assign.mochi
50. [x] list_index.mochi
51. [x] list_nested_assign.mochi
52. [x] list_set_ops.mochi
53. [x] load_jsonl.mochi
54. [x] load_yaml.mochi
55. [x] map_assign.mochi
56. [x] map_in_operator.mochi
57. [x] map_index.mochi
58. [x] map_int_key.mochi
59. [x] map_literal_dynamic.mochi
60. [x] map_membership.mochi
61. [x] map_nested_assign.mochi
62. [x] match_expr.mochi
63. [x] match_full.mochi
64. [x] math_ops.mochi
65. [x] membership.mochi
66. [x] min_max_builtin.mochi
67. [x] mix_go_python.mochi
68. [x] nested_function.mochi
69. [x] order_by_map.mochi
70. [x] outer_join.mochi
71. [ ] pairs_loop.mochi
72. [x] partial_application.mochi
73. [x] print_hello.mochi
74. [x] pure_fold.mochi
75. [x] pure_global_fold.mochi
76. [x] python_auto.mochi
77. [x] python_math.mochi
78. [x] query_sum_select.mochi
79. [x] record_assign.mochi
80. [x] right_join.mochi
81. [x] save_jsonl_stdout.mochi
82. [x] short_circuit.mochi
83. [x] slice.mochi
84. [x] sort_stable.mochi
85. [x] str_builtin.mochi
86. [x] string_compare.mochi
87. [x] string_concat.mochi
88. [x] string_contains.mochi
89. [x] string_in_operator.mochi
90. [x] string_index.mochi
91. [x] string_prefix_slice.mochi
92. [x] substring_builtin.mochi
93. [x] sum_builtin.mochi
94. [x] tail_recursion.mochi
95. [x] test_block.mochi
96. [x] tree_sum.mochi
97. [x] two-sum.mochi
98. [x] typed_let.mochi
99. [x] typed_var.mochi
100. [x] unary_neg.mochi
101. [ ] update_stmt.mochi
102. [x] user_type_literal.mochi
103. [x] values_builtin.mochi
104. [x] var_assignment.mochi
105. [x] while_loop.mochi

Last updated 2025-08-09 10:41 +0700
