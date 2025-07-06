# Round Trip VM Status

- append_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3983880934.ts:3:1
2: 
3: let a: Array<number>;
4:
- avg_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3196013685.ts:11:3
10: function _avg(v: any): number {
11:   let list: any[] | null = null;
12:   if (Array.isArray(v)) list = v;
- basic_compare.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3048123567.ts:3:1
2: 
3: let a: number;
4: let b: number;
- binary_precedence.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-164625777.ts:4:15
3: function main(): void {
4:   console.log(1 + (2 * 3));
5:   console.log((1 + 2) * 3);
- bool_chain.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-806999178.ts:4:15
3: function boom(): boolean {
4:   console.log("boom");
5:   return true;
- break_continue.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-313608437.ts:3:1
2: 
3: let numbers: Array<number>;
4:
- cast_string_to_int.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-2091737668.ts:4:15
3: function main(): void {
4:   console.log("1995");
5: }
- cast_struct.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-813975005.ts:3:1
2: 
3: type Todo = {
4:   title: string;
- closure.mochi: convert error: node: unsupported syntax: FunctionExpression at /tmp/ts-src-1269709255.ts:4:10
3: function makeAdder(n: number): (p0: number) => number {
4:   return function (x: number): number {
5:     return (x + n);
- count_builtin.mochi: convert error: node: unsupported syntax: IfStatement at /tmp/ts-src-17450875.ts:11:3
10: function _count(v: any): number {
11:   if (Array.isArray(v)) return v.length;
12:   if (v && typeof v === "object") {
- cross_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-953634041.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- cross_join_filter.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2827229747.ts:3:1
2: 
3: let letters: Array<string>;
4: let nums: Array<number>;
- cross_join_triple.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-4257533593.ts:3:1
2: 
3: let bools: Array<boolean>;
4: let combos: Array<Record<string, any>>;
- dataset_sort_take_limit.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1190116380.ts:3:1
2: 
3: let expensive: Array<Record<string, any>>;
4: let products: Array<Record<string, any>>;
- dataset_where_filter.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-242247079.ts:3:1
2: 
3: let adults: Array<Record<string, any>>;
4: let people: Array<Record<string, any>>;
- exists_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2073055454.ts:3:1
2: 
3: let data: Array<number>;
4: let flag: boolean;
- for_list_collection.mochi: convert error: node: unsupported syntax: ForOfStatement at /tmp/ts-src-3330214305.ts:4:3
3: function main(): void {
4:   for (
5:     const n of [
- for_loop.mochi: convert error: node: unsupported syntax: ForStatement at /tmp/ts-src-1490070353.ts:4:3
3: function main(): void {
4:   for (let i: number = 1; i < 4; i++) {
5:     console.log(i);
- for_map_collection.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2019968164.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- fun_call.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-2485942108.ts:4:10
3: function add(a: number, b: number): number {
4:   return (a + b);
5: }
- fun_expr_in_let.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2206523629.ts:3:1
2: 
3: let square: (p0: number) => number;
4:
- fun_three_args.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-3394788432.ts:4:10
3: function sum3(a: number, b: number, c: number): number {
4:   return ((a + b) + c);
5: }
- group_by.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2802721039.ts:3:1
2: 
3: let people: Array<Record<string, any>>;
4: let stats: Array<Record<string, any>>;
- group_by_conditional_sum.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3311266783.ts:3:1
2: 
3: let items: Array<Record<string, any>>;
4: let result: Array<Record<string, any>>;
- group_by_having.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2695917487.ts:3:1
2: 
3: let big: Array<Record<string, any>>;
4: let people: Array<Record<string, string>>;
- group_by_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-647010723.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- group_by_left_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2456392022.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- group_by_multi_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1046107924.ts:3:1
2: 
3: let filtered: Array<Record<string, any>>;
4: let grouped: Array<Record<string, any>>;
- group_by_multi_join_sort.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2308344292.ts:3:1
2: 
3: let customer: Array<Record<string, any>>;
4: let end_date: string;
- group_by_sort.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1500897162.ts:3:1
2: 
3: let grouped: Array<Record<string, any>>;
4: let items: Array<Record<string, any>>;
- group_items_iteration.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-394948492.ts:3:1
2: 
3: let data: Array<Record<string, any>>;
4: let groups: Array<any>;
- if_else.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-881492284.ts:3:1
2: 
3: let x: number;
4:
- if_then_else.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-682710680.ts:3:1
2: 
3: let msg: string;
4: let x: number;
- if_then_else_nested.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-246696397.ts:3:1
2: 
3: let msg: string;
4: let x: number;
- in_operator.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2365683033.ts:3:1
2: 
3: let xs: Array<number>;
4:
- in_operator_extended.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3519673949.ts:3:1
2: 
3: let m: Record<string, number>;
4: let s: string;
- inner_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-4221438364.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- join_multi.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1433335011.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let items: Array<Record<string, any>>;
- json_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-173204566.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- left_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2742465309.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- left_join_multi.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1217066091.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let items: Array<Record<string, any>>;
- len_builtin.mochi: convert error: node: unsupported syntax: PropertyAccessExpression at /tmp/ts-src-1508659398.ts:5:5
4:   console.log(
5:     [
6:       1,
- len_map.mochi: convert error: node: unsupported syntax: PropertyAccessExpression at /tmp/ts-src-503683893.ts:5:5
4:   console.log(
5:     Object.keys({
6:       "a": 1,
- len_string.mochi: ok
- let_and_print.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-199661285.ts:3:1
2: 
3: let a: number;
4: let b: number;
- list_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-921668122.ts:3:1
2: 
3: let nums: Array<number>;
4:
- list_index.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3928986031.ts:3:1
2: 
3: let xs: Array<number>;
4:
- list_nested_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-290345648.ts:3:1
2: 
3: let matrix: Array<Array<number>>;
4:
- list_set_ops.mochi: convert error: node: unsupported syntax: PropertyAccessExpression at /tmp/ts-src-2232561176.ts:25:5
24:   console.log(
25:     _union_all([
26:       1,
- load_yaml.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-1217721289.ts:3:1
2: 
3: type Person = {
4:   name: string;
- map_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2748007859.ts:3:1
2: 
3: let scores: Record<string, number>;
4:
- map_in_operator.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2895890259.ts:3:1
2: 
3: let m: Record<number, string>;
4:
- map_index.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1823898256.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- map_int_key.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2973289274.ts:3:1
2: 
3: let m: Record<number, string>;
4:
- map_literal_dynamic.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3299575845.ts:3:1
2: 
3: let m: Record<string, number>;
4: let x: number;
- map_membership.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1286990240.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- map_nested_assign.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1338404035.ts:3:1
2: 
3: let data: Record<string, Record<string, number>>;
4:
- match_expr.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3840151067.ts:3:1
2: 
3: let label: string;
4: let x: number;
- match_full.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1870033693.ts:12:1
11: 
12: let day: string;
13: let label: string;
- math_ops.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-654942447.ts:4:15
3: function main(): void {
4:   console.log(6 * 7);
5:   console.log(Math.trunc(7 / 2));
- membership.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-88353606.ts:3:1
2: 
3: let nums: Array<number>;
4:
- min_max_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2505364237.ts:3:1
2: 
3: let nums: Array<number>;
4:
- nested_function.mochi: convert error: node: unsupported syntax: FunctionDeclaration at /tmp/ts-src-2779599028.ts:4:3
3: function outer(x: number): number {
4:   function inner(y: number): number {
5:     return (x + y);
- order_by_map.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1081997354.ts:3:1
2: 
3: let data: Array<Record<string, number>>;
4: let sorted: Array<Record<string, number>>;
- outer_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3505780178.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- partial_application.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-2350673171.ts:4:10
3: function add(a: number, b: number): number {
4:   return (a + b);
5: }
- print_hello.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-1287759367.ts:4:15
3: function main(): void {
4:   console.log("hello");
5: }
- pure_fold.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-3970928032.ts:4:10
3: function triple(x: number): number {
4:   return (x * 3);
5: }
- pure_global_fold.mochi: convert error: node: unsupported syntax: ParenthesizedExpression at /tmp/ts-src-351051336.ts:4:10
3: function inc(x: number): number {
4:   return (x + k);
5: }
- query_sum_select.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-955398005.ts:3:1
2: 
3: let nums: Array<number>;
4: let result: Array<number>;
- record_assign.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-299136559.ts:3:1
2: 
3: type Counter = {
4:   n: number;
- right_join.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-84967122.ts:3:1
2: 
3: let customers: Array<Record<string, any>>;
4: let orders: Array<Record<string, number>>;
- save_jsonl_stdout.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1650626455.ts:3:1
2: 
3: let people: Array<Record<string, any>>;
4:
- short_circuit.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-2610490715.ts:4:15
3: function boom(a: number, b: number): boolean {
4:   console.log("boom");
5:   return true;
- slice.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-844528550.ts:14:28
13:   ].slice(0, 2));
14:   console.log(_sliceString("hello", 1, 4));
15: }
- sort_stable.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-485648609.ts:3:1
2: 
3: let items: Array<Record<string, any>>;
4: let result: Array<any>;
- str_builtin.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-544466030.ts:4:15
3: function main(): void {
4:   console.log("123");
5: }
- string_compare.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-446596059.ts:4:15
3: function main(): void {
4:   console.log("a" < "b");
5:   console.log("a" <= "a");
- string_concat.mochi: convert error: node: unsupported syntax: BinaryExpression at /tmp/ts-src-2960907612.ts:4:15
3: function main(): void {
4:   console.log("hello " + "world");
5: }
- string_contains.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1310285766.ts:3:1
2: 
3: let s: string;
4:
- string_in_operator.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2508215948.ts:3:1
2: 
3: let s: string;
4:
- string_index.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1888759042.ts:3:1
2: 
3: let s: string;
4:
- string_prefix_slice.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1875205147.ts:3:1
2: 
3: let prefix: string;
4: let s1: string;
- substring_builtin.mochi: convert error: node: unsupported syntax: StringLiteral at /tmp/ts-src-3847081788.ts:4:15
3: function main(): void {
4:   console.log("och");
5: }
- sum_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1414852805.ts:11:3
10: function _sum(v: any): number {
11:   let list: any[] | null = null;
12:   if (Array.isArray(v)) list = v;
- tail_recursion.mochi: convert error: node: unsupported syntax: IfStatement at /tmp/ts-src-4034525668.ts:4:3
3: function sum_rec(n: number, acc: number): number {
4:   if ((n == 0)) {
5:     return acc;
- test_block.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1130287422.ts:4:3
3: function test_addition_works(): void {
4:   let x: number = 1 + 2;
5:   if (!(x == 3)) throw new Error("expect failed");
- tree_sum.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-938247252.ts:3:1
2: 
3: type Leaf = {
4:   __name: "Leaf";
- two-sum.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-3409709712.ts:4:3
3: function twoSum(nums: Array<number>, target: number): Array<number> {
4:   let n: number = nums.length;
5:   for (let i: number = 0; i < n; i++) {
- typed_let.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2938149803.ts:3:1
2: 
3: let y: number;
4:
- typed_var.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-533741912.ts:3:1
2: 
3: let x: number;
4:
- unary_neg.mochi: convert error: node: unsupported syntax: PrefixUnaryExpression at /tmp/ts-src-2248813086.ts:4:15
3: function main(): void {
4:   console.log(-3);
5:   console.log(5 + (-2));
- update_stmt.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-180813367.ts:3:1
2: 
3: type Person = {
4:   name: string;
- user_type_literal.mochi: convert error: node: unsupported syntax: TypeAliasDeclaration at /tmp/ts-src-3773056211.ts:3:1
2: 
3: type Person = {
4:   name: string;
- values_builtin.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-2860875865.ts:3:1
2: 
3: let m: Record<string, number>;
4:
- var_assignment.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1806771213.ts:3:1
2: 
3: let x: number;
4:
- while_loop.mochi: convert error: node: unsupported syntax: FirstStatement at /tmp/ts-src-1255207781.ts:3:1
2: 
3: let i: number;
4:
