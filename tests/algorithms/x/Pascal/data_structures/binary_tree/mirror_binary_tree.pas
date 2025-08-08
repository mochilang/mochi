{$mode objfpc}
program Main;
uses SysUtils;
type Tree = record
  values: array of integer;
  left: array of integer;
  right: array of integer;
  root: integer;
end;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  right: IntArray;
  idx: integer;
  left: IntArray;
  tree_var: Tree;
function makeTree(values: IntArray; left: IntArray; right: IntArray; root: integer): Tree; forward;
procedure mirror_node(left: IntArray; right: IntArray; idx: integer); forward;
function mirror(tree_var: Tree): Tree; forward;
function inorder(tree_var: Tree; idx: integer): IntArray; forward;
function make_tree_zero(): Tree; forward;
function make_tree_seven(): Tree; forward;
function make_tree_nine(): Tree; forward;
procedure main(); forward;
function makeTree(values: IntArray; left: IntArray; right: IntArray; root: integer): Tree;
begin
  Result.values := values;
  Result.left := left;
  Result.right := right;
  Result.root := root;
end;
procedure mirror_node(left: IntArray; right: IntArray; idx: integer);
var
  mirror_node_temp: integer;
begin
  if idx = -1 then begin
  exit();
end;
  mirror_node_temp := left[idx];
  left[idx] := right[idx];
  right[idx] := mirror_node_temp;
  mirror_node(left, right, left[idx]);
  mirror_node(left, right, right[idx]);
end;
function mirror(tree_var: Tree): Tree;
begin
  mirror_node(tree_var.left, tree_var.right, tree_var.root);
  exit(tree_var);
end;
function inorder(tree_var: Tree; idx: integer): IntArray;
var
  inorder_left_vals: array of integer;
  inorder_right_vals: array of integer;
begin
  if idx = -1 then begin
  exit([]);
end;
  inorder_left_vals := inorder(tree_var, tree_var.left[idx]);
  inorder_right_vals := inorder(tree_var, tree_var.right[idx]);
  exit(concat(concat(inorder_left_vals, IntArray([tree_var.values[idx]])), inorder_right_vals));
end;
function make_tree_zero(): Tree;
begin
  exit(makeTree([0], [-1], [-1], 0));
end;
function make_tree_seven(): Tree;
begin
  exit(makeTree([1, 2, 3, 4, 5, 6, 7], [1, 3, 5, -1, -1, -1, -1], [2, 4, 6, -1, -1, -1, -1], 0));
end;
function make_tree_nine(): Tree;
begin
  exit(makeTree([1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 3, -1, 6, -1, -1, -1, -1, -1], [2, 4, 5, 7, 8, -1, -1, -1, -1], 0));
end;
procedure main();
var
  main_names: array of string;
  main_trees: array of Tree;
  main_i: integer;
  main_tree_var: Tree;
  main_mirrored: Tree;
begin
  main_names := ['zero', 'seven', 'nine'];
  main_trees := [make_tree_zero(), make_tree_seven(), make_tree_nine()];
  main_i := 0;
  while main_i < Length(main_trees) do begin
  main_tree_var := main_trees[main_i];
  writeln((('      The ' + main_names[main_i]) + ' tree: ') + list_int_to_str(inorder(main_tree_var, main_tree_var.root)));
  main_mirrored := mirror(main_tree_var);
  writeln((('Mirror of ' + main_names[main_i]) + ' tree: ') + list_int_to_str(inorder(main_mirrored, main_mirrored.root)));
  main_i := main_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
