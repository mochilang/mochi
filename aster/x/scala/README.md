# Scala AST Printer

This package provides a tree-sitter based AST and printer for Scala.

## Golden files

The printer is validated against selected programs under `tests/transpiler/x/scala`.

### Covered examples

- [x] 1. append_builtin.scala
- [x] 2. avg_builtin.scala
- [x] 3. basic_compare.scala
- [x] 4. binary_precedence.scala
- [x] 5. bool_chain.scala
- [x] 6. break_continue.scala
- [x] 7. cast_string_to_int.scala
- [x] 8. cast_struct.scala
- [x] 9. closure.scala
- [x] 10. count_builtin.scala
- [x] 11. cross_join.scala
- [x] 12. cross_join_filter.scala
- [x] 13. cross_join_triple.scala
- [x] 14. dataset_sort_take_limit.scala
- [x] 15. dataset_where_filter.scala
- [x] 16. exists_builtin.scala
- [x] 17. for_list_collection.scala
- [x] 18. for_loop.scala
- [x] 19. for_map_collection.scala
- [x] 20. fun_call.scala
- [x] 21. fun_expr_in_let.scala
- [x] 22. fun_three_args.scala
- [x] 23. go_auto.scala
- [x] 24. group_by.scala
- [x] 25. group_by_conditional_sum.scala
- [x] 26. group_by_having.scala
- [x] 27. group_by_join.scala
- [x] 28. group_by_multi_join.scala
- [x] 29. group_by_multi_join_sort.scala
- [x] 30. group_by_sort.scala
- [x] 31. group_items_iteration.scala
- [x] 32. if_else.scala
- [x] 33. if_then_else.scala
- [x] 34. if_then_else_nested.scala
- [x] 35. in_operator.scala
- [x] 36. in_operator_extended.scala
- [x] 37. inner_join.scala
- [x] 38. join_multi.scala
- [x] 39. json_builtin.scala
- [x] 40. left_join.scala
- [x] 41. left_join_multi.scala
- [x] 42. len_builtin.scala
- [x] 43. len_map.scala
- [x] 44. len_string.scala
- [x] 45. let_and_print.scala
- [x] 46. list_assign.scala
- [x] 47. list_index.scala
- [x] 48. list_nested_assign.scala
- [x] 49. list_set_ops.scala
- [x] 50. load_jsonl.scala
- [x] 51. load_yaml.scala
- [x] 52. map_assign.scala
- [x] 53. map_in_operator.scala
- [x] 54. map_index.scala
- [x] 55. map_int_key.scala
- [x] 56. map_literal_dynamic.scala
- [x] 57. map_membership.scala
- [x] 58. map_nested_assign.scala
- [x] 59. match_expr.scala
- [x] 60. match_full.scala
- [x] 61. math_ops.scala
- [x] 62. membership.scala
- [x] 63. min_max_builtin.scala
- [x] 64. mix_go_python.scala
- [x] 65. nested_function.scala
- [x] 66. order_by_map.scala
- [x] 67. partial_application.scala
- [x] 68. print_hello.scala
- [x] 69. pure_fold.scala
- [x] 70. pure_global_fold.scala
- [x] 71. python_auto.scala
- [x] 72. python_math.scala
- [x] 73. query_sum_select.scala
- [x] 74. record_assign.scala
- [x] 75. right_join.scala
- [x] 76. save_jsonl_stdout.scala
- [x] 77. short_circuit.scala
- [x] 78. slice.scala
- [x] 79. sort_stable.scala
- [x] 80. str_builtin.scala
- [x] 81. string_compare.scala
- [x] 82. string_concat.scala
- [x] 83. string_contains.scala
- [x] 84. string_in_operator.scala
- [x] 85. string_index.scala
- [x] 86. string_prefix_slice.scala
- [x] 87. substring_builtin.scala
- [x] 88. sum_builtin.scala
- [x] 89. tail_recursion.scala
- [x] 90. test_block.scala
- [x] 91. tree_sum.scala
- [x] 92. two-sum.scala
- [x] 93. typed_let.scala
- [x] 94. typed_var.scala
- [x] 95. unary_neg.scala
- [x] 96. update_stmt.scala
- [x] 97. user_type_literal.scala
- [x] 98. values_builtin.scala
- [x] 99. var_assignment.scala
- [x] 100. while_loop.scala
Completed 100 / 100 examples.

_Last updated: 2025-08-01 09:21 GMT+7_

