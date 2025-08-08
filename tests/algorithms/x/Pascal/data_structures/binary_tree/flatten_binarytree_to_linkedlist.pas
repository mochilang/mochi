{$mode objfpc}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  node_data: array of integer;
  left_child: array of integer;
  right_child: array of integer;
  root: integer;
  vals: IntArray;
  value: integer;
  values: IntArray;
function new_node(value: integer): integer; forward;
function build_tree(): integer; forward;
function flatten(root: integer): IntArray; forward;
procedure display(values: IntArray); forward;
function new_node(value: integer): integer;
begin
  node_data := concat(node_data, IntArray([value]));
  left_child := concat(left_child, IntArray([0]));
  right_child := concat(right_child, IntArray([0]));
  exit(Length(node_data) - 1);
end;
function build_tree(): integer;
var
  build_tree_root: integer;
  build_tree_n2: integer;
  build_tree_n5: integer;
  build_tree_n3: integer;
  build_tree_n4: integer;
  build_tree_n6: integer;
begin
  build_tree_root := new_node(1);
  build_tree_n2 := new_node(2);
  build_tree_n5 := new_node(5);
  build_tree_n3 := new_node(3);
  build_tree_n4 := new_node(4);
  build_tree_n6 := new_node(6);
  left_child[build_tree_root] := build_tree_n2;
  right_child[build_tree_root] := build_tree_n5;
  left_child[build_tree_n2] := build_tree_n3;
  right_child[build_tree_n2] := build_tree_n4;
  right_child[build_tree_n5] := build_tree_n6;
  exit(build_tree_root);
end;
function flatten(root: integer): IntArray;
var
  flatten_res: array of integer;
  flatten_left_vals: array of integer;
  flatten_right_vals: array of integer;
  flatten_i: integer;
begin
  if root = 0 then begin
  exit(IntArray([]));
end;
  flatten_res := IntArray([node_data[root]]);
  flatten_left_vals := flatten(left_child[root]);
  flatten_right_vals := flatten(right_child[root]);
  flatten_i := 0;
  while flatten_i < Length(flatten_left_vals) do begin
  flatten_res := concat(flatten_res, IntArray([flatten_left_vals[flatten_i]]));
  flatten_i := flatten_i + 1;
end;
  flatten_i := 0;
  while flatten_i < Length(flatten_right_vals) do begin
  flatten_res := concat(flatten_res, IntArray([flatten_right_vals[flatten_i]]));
  flatten_i := flatten_i + 1;
end;
  exit(flatten_res);
end;
procedure display(values: IntArray);
var
  display_s: string;
  display_i: integer;
begin
  display_s := '';
  display_i := 0;
  while display_i < Length(values) do begin
  if display_i = 0 then begin
  display_s := IntToStr(values[display_i]);
end else begin
  display_s := (display_s + ' ') + IntToStr(values[display_i]);
end;
  display_i := display_i + 1;
end;
  writeln(display_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  node_data := IntArray([0]);
  left_child := IntArray([0]);
  right_child := IntArray([0]);
  writeln('Flattened Linked List:');
  root := build_tree();
  vals := flatten(root);
  display(vals);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
