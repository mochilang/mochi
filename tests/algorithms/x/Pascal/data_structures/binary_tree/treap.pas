{$mode objfpc}
program Main;
uses SysUtils;
type SplitResult = record
  left: integer;
  right: integer;
end;
type RealArray = array of real;
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
  NIL_: integer;
  node_values: array of integer;
  node_priors: array of real;
  node_lefts: array of integer;
  node_rights: array of integer;
  seed: integer;
  value: integer;
  left: integer;
  root: integer;
  acc: IntArray;
  right: integer;
  i: integer;
function makeSplitResult(left: integer; right: integer): SplitResult; forward;
function random(): real; forward;
function new_node(value: integer): integer; forward;
function split(root: integer; value: integer): SplitResult; forward;
function merge(left: integer; right: integer): integer; forward;
function insert(root: integer; value: integer): integer; forward;
function erase(root: integer; value: integer): integer; forward;
function inorder(i: integer; acc: IntArray): IntArray; forward;
procedure main(); forward;
function makeSplitResult(left: integer; right: integer): SplitResult;
begin
  Result.left := left;
  Result.right := right;
end;
function random(): real;
begin
  seed := ((seed * 13) + 7) mod 100;
  exit(Double(seed) / 100);
end;
function new_node(value: integer): integer;
begin
  node_values := concat(node_values, IntArray([value]));
  node_priors := concat(node_priors, [random()]);
  node_lefts := concat(node_lefts, IntArray([NIL_]));
  node_rights := concat(node_rights, IntArray([NIL_]));
  exit(Length(node_values) - 1);
end;
function split(root: integer; value: integer): SplitResult;
var
  split_res: SplitResult;
begin
  if root = NIL_ then begin
  exit(makeSplitResult(NIL_, NIL_));
end;
  if value < node_values[root] then begin
  split_res := split(node_lefts[root], value);
  node_lefts[root] := split_res.right;
  exit(makeSplitResult(split_res.left, root));
end;
  split_res := split(node_rights[root], value);
  node_rights[root] := split_res.left;
  exit(makeSplitResult(root, split_res.right));
end;
function merge(left: integer; right: integer): integer;
begin
  if left = NIL_ then begin
  exit(right);
end;
  if right = NIL_ then begin
  exit(left);
end;
  if node_priors[left] < node_priors[right] then begin
  node_rights[left] := merge(node_rights[left], right);
  exit(left);
end;
  node_lefts[right] := merge(left, node_lefts[right]);
  exit(right);
end;
function insert(root: integer; value: integer): integer;
var
  insert_node: integer;
  insert_res: SplitResult;
begin
  insert_node := new_node(value);
  insert_res := split(root, value);
  exit(merge(merge(insert_res.left, insert_node), insert_res.right));
end;
function erase(root: integer; value: integer): integer;
var
  erase_res1: SplitResult;
  erase_res2: SplitResult;
begin
  erase_res1 := split(root, value - 1);
  erase_res2 := split(erase_res1.right, value);
  exit(merge(erase_res1.left, erase_res2.right));
end;
function inorder(i: integer; acc: IntArray): IntArray;
var
  inorder_left_acc: array of integer;
  inorder_with_node: array of integer;
begin
  if i = NIL_ then begin
  exit(acc);
end;
  inorder_left_acc := inorder(node_lefts[i], acc);
  inorder_with_node := concat(inorder_left_acc, IntArray([node_values[i]]));
  exit(inorder(node_rights[i], inorder_with_node));
end;
procedure main();
var
  main_root: integer;
begin
  main_root := NIL_;
  main_root := insert(main_root, 1);
  writeln(list_int_to_str(inorder(main_root, [])));
  main_root := insert(main_root, 3);
  main_root := insert(main_root, 5);
  main_root := insert(main_root, 17);
  main_root := insert(main_root, 19);
  main_root := insert(main_root, 2);
  main_root := insert(main_root, 16);
  main_root := insert(main_root, 4);
  main_root := insert(main_root, 0);
  writeln(list_int_to_str(inorder(main_root, [])));
  main_root := insert(main_root, 4);
  main_root := insert(main_root, 4);
  main_root := insert(main_root, 4);
  writeln(list_int_to_str(inorder(main_root, [])));
  main_root := erase(main_root, 0);
  writeln(list_int_to_str(inorder(main_root, [])));
  main_root := erase(main_root, 4);
  writeln(list_int_to_str(inorder(main_root, [])));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  NIL_ := 0 - 1;
  node_values := [];
  node_priors := [];
  node_lefts := [];
  node_rights := [];
  seed := 1;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
