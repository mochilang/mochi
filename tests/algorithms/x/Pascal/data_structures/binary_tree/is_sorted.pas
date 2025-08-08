{$mode objfpc}
program Main;
uses SysUtils;
type Tree = record
  data: array of real;
  left: array of integer;
  right: array of integer;
end;
type RealArray = array of real;
type IntArray = array of integer;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  NONE: integer;
  tree1: Tree;
  tree2: Tree;
  tree3: Tree;
  index: integer;
  tree_var: Tree;
function makeTree(data: RealArray; left: IntArray; right: IntArray): Tree; forward;
function inorder(tree_var: Tree; index: integer): RealArray; forward;
function is_sorted(tree_var: Tree; index: integer): boolean; forward;
function makeTree(data: RealArray; left: IntArray; right: IntArray): Tree;
begin
  Result.data := data;
  Result.left := left;
  Result.right := right;
end;
function inorder(tree_var: Tree; index: integer): RealArray;
var
  inorder_res: array of real;
  inorder_left_idx: integer;
  inorder_right_idx: integer;
begin
  inorder_res := [];
  if index = NONE then begin
  exit(inorder_res);
end;
  inorder_left_idx := tree_var.left[index];
  if inorder_left_idx <> NONE then begin
  inorder_res := concat(inorder_res, inorder(tree_var, inorder_left_idx));
end;
  inorder_res := concat(inorder_res, [tree_var.data[index]]);
  inorder_right_idx := tree_var.right[index];
  if inorder_right_idx <> NONE then begin
  inorder_res := concat(inorder_res, inorder(tree_var, inorder_right_idx));
end;
  exit(inorder_res);
end;
function is_sorted(tree_var: Tree; index: integer): boolean;
var
  is_sorted_left_idx: integer;
  is_sorted_right_idx: integer;
begin
  if index = NONE then begin
  exit(true);
end;
  is_sorted_left_idx := tree_var.left[index];
  if is_sorted_left_idx <> NONE then begin
  if tree_var.data[index] < tree_var.data[is_sorted_left_idx] then begin
  exit(false);
end;
  if not is_sorted(tree_var, is_sorted_left_idx) then begin
  exit(false);
end;
end;
  is_sorted_right_idx := tree_var.right[index];
  if is_sorted_right_idx <> NONE then begin
  if tree_var.data[index] > tree_var.data[is_sorted_right_idx] then begin
  exit(false);
end;
  if not is_sorted(tree_var, is_sorted_right_idx) then begin
  exit(false);
end;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  NONE := 0 - 1;
  tree1 := makeTree([2.1, 2, 2.2], [1, NONE, NONE], [2, NONE, NONE]);
  writeln((('Tree ' + list_real_to_str(inorder(tree1, 0))) + ' is sorted: ') + LowerCase(BoolToStr(is_sorted(tree1, 0), true)));
  tree2 := makeTree([2.1, 2, 2], [1, NONE, NONE], [2, NONE, NONE]);
  writeln((('Tree ' + list_real_to_str(inorder(tree2, 0))) + ' is sorted: ') + LowerCase(BoolToStr(is_sorted(tree2, 0), true)));
  tree3 := makeTree([2.1, 2, 2.1], [1, NONE, NONE], [2, NONE, NONE]);
  writeln((('Tree ' + list_real_to_str(inorder(tree3, 0))) + ' is sorted: ') + LowerCase(BoolToStr(is_sorted(tree3, 0), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
