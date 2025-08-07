{$mode objfpc}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  symmetric_tree: IntArrayArray;
  asymmetric_tree: IntArrayArray;
  tree: IntArrayArray;
function make_symmetric_tree(): IntArrayArray; forward;
function make_asymmetric_tree(): IntArrayArray; forward;
function is_symmetric_tree(tree: IntArrayArray): boolean; forward;
function make_symmetric_tree(): IntArrayArray;
begin
  exit([[1, 1, 2], [2, 3, 4], [2, 5, 6], [3, -1, -1], [4, -1, -1], [4, -1, -1], [3, -1, -1]]);
end;
function make_asymmetric_tree(): IntArrayArray;
begin
  exit([[1, 1, 2], [2, 3, 4], [2, 5, 6], [3, -1, -1], [4, -1, -1], [3, -1, -1], [4, -1, -1]]);
end;
function is_symmetric_tree(tree: IntArrayArray): boolean;
var
  is_symmetric_tree_stack: array of integer;
  is_symmetric_tree_left: integer;
  is_symmetric_tree_right: integer;
  is_symmetric_tree_lnode: array of integer;
  is_symmetric_tree_rnode: array of integer;
begin
  is_symmetric_tree_stack := [tree[0][1], tree[0][2]];
  while Length(is_symmetric_tree_stack) >= 2 do begin
  is_symmetric_tree_left := is_symmetric_tree_stack[Length(is_symmetric_tree_stack) - 2];
  is_symmetric_tree_right := is_symmetric_tree_stack[Length(is_symmetric_tree_stack) - 1];
  is_symmetric_tree_stack := copy(is_symmetric_tree_stack, 0, (Length(is_symmetric_tree_stack) - 2 - (0)));
  if (is_symmetric_tree_left = -1) and (is_symmetric_tree_right = -1) then begin
  continue;
end;
  if (is_symmetric_tree_left = -1) or (is_symmetric_tree_right = -1) then begin
  exit(false);
end;
  is_symmetric_tree_lnode := tree[is_symmetric_tree_left];
  is_symmetric_tree_rnode := tree[is_symmetric_tree_right];
  if is_symmetric_tree_lnode[0] <> is_symmetric_tree_rnode[0] then begin
  exit(false);
end;
  is_symmetric_tree_stack := concat(is_symmetric_tree_stack, IntArray([is_symmetric_tree_lnode[1]]));
  is_symmetric_tree_stack := concat(is_symmetric_tree_stack, IntArray([is_symmetric_tree_rnode[2]]));
  is_symmetric_tree_stack := concat(is_symmetric_tree_stack, IntArray([is_symmetric_tree_lnode[2]]));
  is_symmetric_tree_stack := concat(is_symmetric_tree_stack, IntArray([is_symmetric_tree_rnode[1]]));
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  symmetric_tree := make_symmetric_tree();
  asymmetric_tree := make_asymmetric_tree();
  writeln(LowerCase(BoolToStr(is_symmetric_tree(symmetric_tree), true)));
  writeln(LowerCase(BoolToStr(is_symmetric_tree(asymmetric_tree), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
