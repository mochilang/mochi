# Round Trip VM Status

- append_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2939491427.ts:3:1
2: 
3: let a: Array<number>;
4:
- avg_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2196386894.ts:11:3
10: function _avg(v: any): number {
11:   let list: any[] | null = null;
12:   if (Array.isArray(v)) list = v;
- basic_compare.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-915212826.ts:3:1
2: 
3: let a: number;
4: let b: number;
- binary_precedence.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-1066645028.ts:4:15
3: function main(): void {
4:   console.log(1 + (2 * 3));
5:   console.log((1 + 2) * 3);
- bool_chain.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-799557578.ts:4:15
3: function boom(): boolean {
4:   console.log("boom");
5:   return true;
- break_continue.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1211425000.ts:3:1
2: 
3: let numbers: Array<number>;
4:
- cast_string_to_int.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-3987416117.ts:4:15
3: function main(): void {
4:   console.log("1995");
5: }
- cast_struct.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-3445305070.ts:3:1
2: 
3: type Todo = {
4:   title: string;
- closure.mochi: convert error: node: unsupported syntax: FunctionExpression at /tmp/ts-src-2759197829.ts:4:10
3: function makeAdder(n: number): (p0: number) => number {
4:   return function (x: number): number {
5:     return (x + n);
- count_builtin.mochi: convert error: node: unsupported syntax: IfStatement at /tmp/ts-src-2837113396.ts:11:3
10: function _count(v: any): number {
11:   if (Array.isArray(v)) return v.length;
12:   if (v && typeof v === "object") {
- cross_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-646968600.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- cross_join_filter.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2796687591.ts:3:1
2: 
3: let letters: Array<string>;
4: let nums: Array<number>;
- cross_join_triple.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1329978714.ts:3:1
2: 
3: let bools: Array<boolean>;
4: let combos: Array<Record<string, any>>;
- dataset_sort_take_limit.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1519667102.ts:3:1
2: 
3: let expensive: Array<Record<string, any>>;
4: let products: Array<Record<string, any>>;
- dataset_where_filter.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3221386450.ts:3:1
2: 
3: let adults: Array<Record<string, any>>;
4: let people: Array<Record<string, any>>;
- exists_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3871239147.ts:3:1
2: 
3: let data: Array<number>;
4: let flag: boolean;
- for_list_collection.mochi: convert error: node: unsupported syntax: ForOfStatement at /tmp/ts-src-3074836390.ts:4:3
3: function main(): void {
4:   for (
5:     const n of [
- for_loop.mochi: convert error: node: unsupported syntax: ForStatement at /tmp/ts-src-1305480499.ts:4:3
3: function main(): void {
4:   for (let i: number = 1; i < 4; i++) {
5:     console.log(i);
- for_map_collection.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3326879520.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- fun_call.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-839167463.ts:4:10
3: function add(a: number, b: number): number {
4:   return (a + b);
5: }
- fun_expr_in_let.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2572091953.ts:3:1
2: 
3: let square: (p0: number) => number;
4:
- fun_three_args.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-1120323616.ts:4:10
3: function sum3(a: number, b: number, c: number): number {
4:   return ((a + b) + c);
5: }
- group_by.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-708595958.ts:3:1
2: 
3: let people: Array<Record<string, any>>;
4: let stats: Array<Record<string, any>>;
- group_by_conditional_sum.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1744330317.ts:3:1
2: 
3: let items: Array<Record<string, any>>;
4: let result: Array<Record<string, any>>;
- group_by_having.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1146856342.ts:3:1
2: 
3: let big: Array<Record<string, any>>;
4: let people: Array<Record<string, string>>;
- group_by_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-253503745.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- group_by_left_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2559071804.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- group_by_multi_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1797590673.ts:3:1
2: 
3: let filtered: Array<Record<string, any>>;
4: let grouped: Array<Record<string, any>>;
- group_by_multi_join_sort.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3015540577.ts:3:1
2: 
3: let customer: Array<Record<string, any>>;
4: let end_date: string;
- group_by_sort.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2349599883.ts:3:1
2: 
3: let grouped: Array<Record<string, any>>;
4: let items: Array<Record<string, any>>;
- group_items_iteration.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2602866808.ts:3:1
2: 
3: let data: Array<Record<string, any>>;
4: let groups: Array<any>;
- if_else.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1780802840.ts:3:1
2: 
3: let x: number;
4:
- if_then_else.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3754989162.ts:3:1
2: 
3: let msg: string;
4: let x: number;
- if_then_else_nested.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2082762153.ts:3:1
2: 
3: let msg: string;
4: let x: number;
- in_operator.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3300454138.ts:3:1
2: 
3: let xs: Array<number>;
4:
- in_operator_extended.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-664300542.ts:3:1
2: 
3: let m: Record<string, number>;
4: let s: string;
- inner_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2348355871.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- join_multi.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-426573761.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let items: Array<Record<string, any>>;
- json_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2362831918.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- left_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3093843604.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- left_join_multi.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1919026895.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let items: Array<Record<string, any>>;
- len_builtin.mochi: convert error: node: unsupported syntax: PropertyAccessExpression at /tmp/ts-src-3786148640.ts:5:5
4:   console.log(
5:     [
6:       1,
- len_map.mochi: convert error: node: unsupported syntax: PropertyAccessExpression at /tmp/ts-src-3757725385.ts:5:5
4:   console.log(
5:     Object.keys({
6:       "a": 1,
- len_string.mochi: ok
- let_and_print.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-4164765527.ts:3:1
2: 
3: let a: number;
4: let b: number;
- list_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1274425403.ts:3:1
2: 
3: let nums: Array<number>;
4:
- list_index.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-784673380.ts:3:1
2: 
3: let xs: Array<number>;
4:
- list_nested_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-983538064.ts:3:1
2: 
3: let matrix: Array<Array<number>>;
4:
- list_set_ops.mochi: convert error: node: unsupported syntax: PropertyAccessExpression at /tmp/ts-src-3522914734.ts:25:5
24:   console.log(
25:     _union_all([
26:       1,
- load_yaml.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-2274547056.ts:3:1
2: 
3: type Person = {
4:   name: string;
- map_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1074556527.ts:3:1
2: 
3: let scores: Record<string, number>;
4:
- map_in_operator.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-695734374.ts:3:1
2: 
3: let m: Record<number, string>;
4:
- map_index.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-4191362603.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- map_int_key.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1883943020.ts:3:1
2: 
3: let m: Record<number, string>;
4:
- map_literal_dynamic.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3635708957.ts:3:1
2: 
3: let m: Record<string, number>;
4: let x: number;
- map_membership.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-4171770055.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- map_nested_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3082201658.ts:3:1
2: 
3: let data: Record<string, Record<string, number>>;
4:
- match_expr.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3447776464.ts:3:1
2: 
3: let label: string;
4: let x: number;
- match_full.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3098398920.ts:12:1
11: 
12: let day: string;
13: let label: string;
- math_ops.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-2186825015.ts:4:15
3: function main(): void {
4:   console.log(6 * 7);
5:   console.log(Math.trunc(7 / 2));
- membership.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1977090655.ts:3:1
2: 
3: let nums: Array<number>;
4:
- min_max_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1460551671.ts:3:1
2: 
3: let nums: Array<number>;
4:
- nested_function.mochi: convert error: node: unsupported syntax: FunctionDeclaration at /tmp/ts-src-83386813.ts:4:3
3: function outer(x: number): number {
4:   function inner(y: number): number {
5:     return (x + y);
- order_by_map.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2818947500.ts:3:1
2: 
3: let data: Array<Record<string, number>>;
4: let sorted: Array<Record<string, number>>;
- outer_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-165932679.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- partial_application.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-2612596431.ts:4:10
3: function add(a: number, b: number): number {
4:   return (a + b);
5: }
- print_hello.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-771946854.ts:4:15
3: function main(): void {
4:   console.log("hello");
5: }
- pure_fold.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-4200726440.ts:4:10
3: function triple(x: number): number {
4:   return (x * 3);
5: }
- pure_global_fold.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-1801342705.ts:4:10
3: function inc(x: number): number {
4:   return (x + k);
5: }
- query_sum_select.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2535093109.ts:3:1
2: 
3: let nums: Array<number>;
4: let result: Array<number>;
- record_assign.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-3513779449.ts:3:1
2: 
3: type Counter = {
4:   n: number;
- right_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1227114547.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- save_jsonl_stdout.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3267133840.ts:3:1
2: 
3: let people: Array<Record<string, any>>;
4:
- short_circuit.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-5712173.ts:4:15
3: function boom(a: number, b: number): boolean {
4:   console.log("boom");
5:   return true;
- slice.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-2526011504.ts:14:28
13:   ].slice(0, 2));
14:   console.log(_sliceString("hello", 1, 4));
15: }
- sort_stable.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3243819482.ts:3:1
2: 
3: let items: Array<Record<string, any>>;
4: let result: Array<any>;
- str_builtin.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-3426273065.ts:4:15
3: function main(): void {
4:   console.log("123");
5: }
- string_compare.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-1709430599.ts:4:15
3: function main(): void {
4:   console.log("a" < "b");
5:   console.log("a" <= "a");
- string_concat.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-3293659223.ts:4:15
3: function main(): void {
4:   console.log("hello " + "world");
5: }
- string_contains.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3803085893.ts:3:1
2: 
3: let s: string;
4:
- string_in_operator.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-121339119.ts:3:1
2: 
3: let s: string;
4:
- string_index.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-635320960.ts:3:1
2: 
3: let s: string;
4:
- string_prefix_slice.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2256482631.ts:3:1
2: 
3: let prefix: string;
4: let s1: string;
- substring_builtin.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-2652219537.ts:4:15
3: function main(): void {
4:   console.log("och");
5: }
- sum_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-700118451.ts:11:3
10: function _sum(v: any): number {
11:   let list: any[] | null = null;
12:   if (Array.isArray(v)) list = v;
- tail_recursion.mochi: convert error: node: unsupported syntax: IfStatement at /tmp/ts-src-4081864236.ts:4:3
3: function sum_rec(n: number, acc: number): number {
4:   if ((n == 0)) {
5:     return acc;
- test_block.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-74621413.ts:4:3
3: function test_addition_works(): void {
4:   let x: number = 1 + 2;
5:   if (!(x == 3)) throw new Error("expect failed");
- tree_sum.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-2047649728.ts:3:1
2: 
3: type Leaf = {
4:   __name: "Leaf";
- two-sum.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3272650839.ts:4:3
3: function twoSum(nums: Array<number>, target: number): Array<number> {
4:   let n: number = nums.length;
5:   for (let i: number = 0; i < n; i++) {
- typed_let.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2696325712.ts:3:1
2: 
3: let y: number;
4:
- typed_var.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3645068618.ts:3:1
2: 
3: let x: number;
4:
- unary_neg.mochi: convert error: node: unsupported syntax: PrefixUnaryExpression at /tmp/ts-src-50836199.ts:4:15
3: function main(): void {
4:   console.log(-3);
5:   console.log(5 + (-2));
- update_stmt.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-3760819688.ts:3:1
2: 
3: type Person = {
4:   name: string;
- user_type_literal.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-3376093275.ts:3:1
2: 
3: type Person = {
4:   name: string;
- values_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1420265129.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- var_assignment.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3017791760.ts:3:1
2: 
3: let x: number;
4:
- while_loop.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3032253504.ts:3:1
2: 
3: let i: number;
4:
