# Erlang AST Printer

This directory contains utilities for inspecting Erlang source code using Tree-sitter and printing it back from the parsed AST. Golden tests verify that the printed source matches the original program output.

Last updated: 2025-07-31 19:13 GMT+7

## Golden Test Checklist (50/50)
1. [x] append_builtin.erl
2. [x] avg_builtin.erl
3. [x] basic_compare.erl
4. [x] bench_block.erl
5. [x] binary_precedence.erl
6. [x] bool_chain.erl
7. [x] break_continue.erl
8. [x] cast_string_to_int.erl
9. [x] cast_struct.erl
10. [x] closure.erl
11. [x] count_builtin.erl
12. [x] cross_join.erl
13. [x] cross_join_filter.erl
14. [x] cross_join_triple.erl
15. [x] dataset_sort_take_limit.erl
16. [x] dataset_where_filter.erl
17. [x] exists_builtin.erl
18. [x] for_list_collection.erl
19. [x] for_loop.erl
20. [x] for_map_collection.erl
21. [x] fun_call.erl
22. [x] fun_expr_in_let.erl
23. [x] fun_three_args.erl
24. [x] go_auto.erl
25. [x] group_by.erl
26. [x] group_by_conditional_sum.erl
27. [x] group_by_having.erl
28. [x] group_by_join.erl
29. [x] group_by_left_join.erl
30. [x] group_by_multi_join.erl
31. [x] group_by_multi_join_sort.erl
32. [x] group_by_multi_sort.erl
33. [x] group_by_sort.erl
34. [x] group_items_iteration.erl
35. [x] if_else.erl
36. [x] if_then_else.erl
37. [x] if_then_else_nested.erl
38. [x] in_operator.erl
39. [x] in_operator_extended.erl
40. [x] inner_join.erl
41. [x] join_multi.erl
42. [x] json_builtin.erl
43. [x] left_join.erl
44. [x] left_join_multi.erl
45. [x] len_builtin.erl
46. [x] len_map.erl
47. [x] len_string.erl
48. [x] let_and_print.erl
49. [x] list_assign.erl
50. [x] list_index.erl
