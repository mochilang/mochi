{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type RBTree = record
  nodes: array of IntArray;
  root: integer;
end;
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
  LABEL_: integer;
  COLOR: integer;
  PARENT: integer;
  LEFT: integer;
  RIGHT: integer;
  NEG_ONE: integer;
  x: integer;
  t: RBTree;
  z: integer;
  v: integer;
  acc: IntArray;
function makeRBTree(nodes: IntArrayArray; root: integer): RBTree; forward;
function make_tree(): RBTree; forward;
function rotate_left(t: RBTree; x: integer): RBTree; forward;
function rotate_right(t: RBTree; x: integer): RBTree; forward;
function insert_fix(t: RBTree; z: integer): RBTree; forward;
function tree_insert(t: RBTree; v: integer): RBTree; forward;
function inorder(t: RBTree; x: integer; acc: IntArray): IntArray; forward;
procedure main(); forward;
function makeRBTree(nodes: IntArrayArray; root: integer): RBTree;
begin
  Result.nodes := nodes;
  Result.root := root;
end;
function make_tree(): RBTree;
begin
  exit(makeRBTree([], -1));
end;
function rotate_left(t: RBTree; x: integer): RBTree;
var
  rotate_left_nodes: array of IntArray;
  rotate_left_y: integer;
  rotate_left_yLeft: integer;
  rotate_left_xParent: integer;
begin
  rotate_left_nodes := t.nodes;
  rotate_left_y := rotate_left_nodes[x][RIGHT];
  rotate_left_yLeft := rotate_left_nodes[rotate_left_y][LEFT];
  rotate_left_nodes[x][RIGHT] := rotate_left_yLeft;
  if rotate_left_yLeft <> NEG_ONE then begin
  rotate_left_nodes[rotate_left_yLeft][PARENT] := x;
end;
  rotate_left_xParent := rotate_left_nodes[x][PARENT];
  rotate_left_nodes[rotate_left_y][PARENT] := rotate_left_xParent;
  if rotate_left_xParent = NEG_ONE then begin
  t.root := rotate_left_y;
end else begin
  if x = rotate_left_nodes[rotate_left_xParent][LEFT] then begin
  rotate_left_nodes[rotate_left_xParent][LEFT] := rotate_left_y;
end else begin
  rotate_left_nodes[rotate_left_xParent][RIGHT] := rotate_left_y;
end;
end;
  rotate_left_nodes[rotate_left_y][LEFT] := x;
  rotate_left_nodes[x][PARENT] := rotate_left_y;
  t.nodes := rotate_left_nodes;
  exit(t);
end;
function rotate_right(t: RBTree; x: integer): RBTree;
var
  rotate_right_nodes: array of IntArray;
  rotate_right_y: integer;
  rotate_right_yRight: integer;
  rotate_right_xParent: integer;
begin
  rotate_right_nodes := t.nodes;
  rotate_right_y := rotate_right_nodes[x][LEFT];
  rotate_right_yRight := rotate_right_nodes[rotate_right_y][RIGHT];
  rotate_right_nodes[x][LEFT] := rotate_right_yRight;
  if rotate_right_yRight <> NEG_ONE then begin
  rotate_right_nodes[rotate_right_yRight][PARENT] := x;
end;
  rotate_right_xParent := rotate_right_nodes[x][PARENT];
  rotate_right_nodes[rotate_right_y][PARENT] := rotate_right_xParent;
  if rotate_right_xParent = NEG_ONE then begin
  t.root := rotate_right_y;
end else begin
  if x = rotate_right_nodes[rotate_right_xParent][RIGHT] then begin
  rotate_right_nodes[rotate_right_xParent][RIGHT] := rotate_right_y;
end else begin
  rotate_right_nodes[rotate_right_xParent][LEFT] := rotate_right_y;
end;
end;
  rotate_right_nodes[rotate_right_y][RIGHT] := x;
  rotate_right_nodes[x][PARENT] := rotate_right_y;
  t.nodes := rotate_right_nodes;
  exit(t);
end;
function insert_fix(t: RBTree; z: integer): RBTree;
var
  insert_fix_nodes: array of IntArray;
  insert_fix_y: integer;
  insert_fix_gp: integer;
begin
  insert_fix_nodes := t.nodes;
  while (z <> t.root) and (insert_fix_nodes[insert_fix_nodes[z][PARENT]][COLOR] = 1) do begin
  if insert_fix_nodes[z][PARENT] = insert_fix_nodes[insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT]][LEFT] then begin
  insert_fix_y := insert_fix_nodes[insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT]][RIGHT];
  if (insert_fix_y <> NEG_ONE) and (insert_fix_nodes[insert_fix_y][COLOR] = 1) then begin
  insert_fix_nodes[insert_fix_nodes[z][PARENT]][COLOR] := 0;
  insert_fix_nodes[insert_fix_y][COLOR] := 0;
  insert_fix_gp := insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT];
  insert_fix_nodes[insert_fix_gp][COLOR] := 1;
  z := insert_fix_gp;
end else begin
  if z = insert_fix_nodes[insert_fix_nodes[z][PARENT]][RIGHT] then begin
  z := insert_fix_nodes[z][PARENT];
  t.nodes := insert_fix_nodes;
  t := rotate_left(t, z);
  insert_fix_nodes := t.nodes;
end;
  insert_fix_nodes[insert_fix_nodes[z][PARENT]][COLOR] := 0;
  insert_fix_gp := insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT];
  insert_fix_nodes[insert_fix_gp][COLOR] := 1;
  t.nodes := insert_fix_nodes;
  t := rotate_right(t, insert_fix_gp);
  insert_fix_nodes := t.nodes;
end;
end else begin
  insert_fix_y := insert_fix_nodes[insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT]][LEFT];
  if (insert_fix_y <> NEG_ONE) and (insert_fix_nodes[insert_fix_y][COLOR] = 1) then begin
  insert_fix_nodes[insert_fix_nodes[z][PARENT]][COLOR] := 0;
  insert_fix_nodes[insert_fix_y][COLOR] := 0;
  insert_fix_gp := insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT];
  insert_fix_nodes[insert_fix_gp][COLOR] := 1;
  z := insert_fix_gp;
end else begin
  if z = insert_fix_nodes[insert_fix_nodes[z][PARENT]][LEFT] then begin
  z := insert_fix_nodes[z][PARENT];
  t.nodes := insert_fix_nodes;
  t := rotate_right(t, z);
  insert_fix_nodes := t.nodes;
end;
  insert_fix_nodes[insert_fix_nodes[z][PARENT]][COLOR] := 0;
  insert_fix_gp := insert_fix_nodes[insert_fix_nodes[z][PARENT]][PARENT];
  insert_fix_nodes[insert_fix_gp][COLOR] := 1;
  t.nodes := insert_fix_nodes;
  t := rotate_left(t, insert_fix_gp);
  insert_fix_nodes := t.nodes;
end;
end;
end;
  insert_fix_nodes := t.nodes;
  insert_fix_nodes[t.root][COLOR] := 0;
  t.nodes := insert_fix_nodes;
  exit(t);
end;
function tree_insert(t: RBTree; v: integer): RBTree;
var
  tree_insert_nodes: array of IntArray;
  tree_insert_node: array of integer;
  tree_insert_idx: integer;
  tree_insert_y: integer;
  tree_insert_x: integer;
begin
  tree_insert_nodes := t.nodes;
  tree_insert_node := [v, 1, -1, -1, -1];
  tree_insert_nodes := concat(tree_insert_nodes, [tree_insert_node]);
  tree_insert_idx := Length(tree_insert_nodes) - 1;
  tree_insert_y := NEG_ONE;
  tree_insert_x := t.root;
  while tree_insert_x <> NEG_ONE do begin
  tree_insert_y := tree_insert_x;
  if v < tree_insert_nodes[tree_insert_x][LABEL_] then begin
  tree_insert_x := tree_insert_nodes[tree_insert_x][LEFT];
end else begin
  tree_insert_x := tree_insert_nodes[tree_insert_x][RIGHT];
end;
end;
  tree_insert_nodes[tree_insert_idx][PARENT] := tree_insert_y;
  if tree_insert_y = NEG_ONE then begin
  t.root := tree_insert_idx;
end else begin
  if v < tree_insert_nodes[tree_insert_y][LABEL_] then begin
  tree_insert_nodes[tree_insert_y][LEFT] := tree_insert_idx;
end else begin
  tree_insert_nodes[tree_insert_y][RIGHT] := tree_insert_idx;
end;
end;
  t.nodes := tree_insert_nodes;
  t := insert_fix(t, tree_insert_idx);
  exit(t);
end;
function inorder(t: RBTree; x: integer; acc: IntArray): IntArray;
begin
  if x = NEG_ONE then begin
  exit(acc);
end;
  acc := inorder(t, t.nodes[x][LEFT], acc);
  acc := concat(acc, IntArray([t.nodes[x][LABEL_]]));
  acc := inorder(t, t.nodes[x][RIGHT], acc);
  exit(acc);
end;
procedure main();
var
  main_t: RBTree;
  main_values: array of integer;
  main_i: integer;
  main_res: array of integer;
begin
  main_t := make_tree();
  main_values := [10, 20, 30, 15, 25, 5, 1];
  main_i := 0;
  while main_i < Length(main_values) do begin
  main_t := tree_insert(main_t, main_values[main_i]);
  main_i := main_i + 1;
end;
  main_res := [];
  main_res := inorder(main_t, main_t.root, main_res);
  writeln(list_int_to_str(main_res));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LABEL_ := 0;
  COLOR := 1;
  PARENT := 2;
  LEFT := 3;
  RIGHT := 4;
  NEG_ONE := -1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
