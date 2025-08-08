{$mode objfpc}
program Main;
uses SysUtils;
type FenwickTree = record
  size: integer;
  tree: array of integer;
end;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  f_base: FenwickTree;
  f: FenwickTree;
  f2: FenwickTree;
  f3: FenwickTree;
  size: integer;
  b: integer;
  right: integer;
  left: integer;
  arr: IntArray;
  x: integer;
  a: integer;
  value: integer;
  index: integer;
function makeFenwickTree(size: integer; tree: IntArray): FenwickTree; forward;
function fenwick_from_list(arr: IntArray): FenwickTree; forward;
function fenwick_empty(size: integer): FenwickTree; forward;
function fenwick_get_array(f: FenwickTree): IntArray; forward;
function bit_and(a: integer; b: integer): integer; forward;
function low_bit(x: integer): integer; forward;
function fenwick_next(index: integer): integer; forward;
function fenwick_prev(index: integer): integer; forward;
function fenwick_add(f: FenwickTree; index: integer; value: integer): FenwickTree; forward;
function fenwick_update(f: FenwickTree; index: integer; value: integer): FenwickTree; forward;
function fenwick_prefix(f: FenwickTree; right: integer): integer; forward;
function fenwick_query(f: FenwickTree; left: integer; right: integer): integer; forward;
function fenwick_get(f: FenwickTree; index: integer): integer; forward;
function fenwick_rank_query(f: FenwickTree; value: integer): integer; forward;
function makeFenwickTree(size: integer; tree: IntArray): FenwickTree;
begin
  Result.size := size;
  Result.tree := tree;
end;
function fenwick_from_list(arr: IntArray): FenwickTree;
var
  fenwick_from_list_size: integer;
  fenwick_from_list_tree: array of integer;
  fenwick_from_list_i: integer;
  fenwick_from_list_j: integer;
begin
  fenwick_from_list_size := Length(arr);
  fenwick_from_list_tree := [];
  fenwick_from_list_i := 0;
  while fenwick_from_list_i < fenwick_from_list_size do begin
  fenwick_from_list_tree := concat(fenwick_from_list_tree, IntArray([arr[fenwick_from_list_i]]));
  fenwick_from_list_i := fenwick_from_list_i + 1;
end;
  fenwick_from_list_i := 1;
  while fenwick_from_list_i < fenwick_from_list_size do begin
  fenwick_from_list_j := fenwick_next(fenwick_from_list_i);
  if fenwick_from_list_j < fenwick_from_list_size then begin
  fenwick_from_list_tree[fenwick_from_list_j] := fenwick_from_list_tree[fenwick_from_list_j] + fenwick_from_list_tree[fenwick_from_list_i];
end;
  fenwick_from_list_i := fenwick_from_list_i + 1;
end;
  exit(makeFenwickTree(fenwick_from_list_size, fenwick_from_list_tree));
end;
function fenwick_empty(size: integer): FenwickTree;
var
  fenwick_empty_tree: array of integer;
  fenwick_empty_i: integer;
begin
  fenwick_empty_tree := [];
  fenwick_empty_i := 0;
  while fenwick_empty_i < size do begin
  fenwick_empty_tree := concat(fenwick_empty_tree, IntArray([0]));
  fenwick_empty_i := fenwick_empty_i + 1;
end;
  exit(makeFenwickTree(size, fenwick_empty_tree));
end;
function fenwick_get_array(f: FenwickTree): IntArray;
var
  fenwick_get_array_arr: array of integer;
  fenwick_get_array_i: integer;
  fenwick_get_array_j: integer;
begin
  fenwick_get_array_arr := [];
  fenwick_get_array_i := 0;
  while fenwick_get_array_i < f.size do begin
  fenwick_get_array_arr := concat(fenwick_get_array_arr, IntArray([f.tree[fenwick_get_array_i]]));
  fenwick_get_array_i := fenwick_get_array_i + 1;
end;
  fenwick_get_array_i := f.size - 1;
  while fenwick_get_array_i > 0 do begin
  fenwick_get_array_j := fenwick_next(fenwick_get_array_i);
  if fenwick_get_array_j < f.size then begin
  fenwick_get_array_arr[fenwick_get_array_j] := fenwick_get_array_arr[fenwick_get_array_j] - fenwick_get_array_arr[fenwick_get_array_i];
end;
  fenwick_get_array_i := fenwick_get_array_i - 1;
end;
  exit(fenwick_get_array_arr);
end;
function bit_and(a: integer; b: integer): integer;
var
  bit_and_ua: integer;
  bit_and_ub: integer;
  bit_and_res: integer;
  bit_and_bit: integer;
begin
  bit_and_ua := a;
  bit_and_ub := b;
  bit_and_res := 0;
  bit_and_bit := 1;
  while (bit_and_ua <> 0) or (bit_and_ub <> 0) do begin
  if ((bit_and_ua mod 2) = 1) and ((bit_and_ub mod 2) = 1) then begin
  bit_and_res := bit_and_res + bit_and_bit;
end;
  bit_and_ua := Trunc(bit_and_ua div 2);
  bit_and_ub := Trunc(bit_and_ub div 2);
  bit_and_bit := bit_and_bit * 2;
end;
  exit(bit_and_res);
end;
function low_bit(x: integer): integer;
begin
  if x = 0 then begin
  exit(0);
end;
  exit(x - bit_and(x, x - 1));
end;
function fenwick_next(index: integer): integer;
begin
  exit(index + low_bit(index));
end;
function fenwick_prev(index: integer): integer;
begin
  exit(index - low_bit(index));
end;
function fenwick_add(f: FenwickTree; index: integer; value: integer): FenwickTree;
var
  fenwick_add_tree: array of integer;
  fenwick_add_i: integer;
begin
  fenwick_add_tree := f.tree;
  if index = 0 then begin
  fenwick_add_tree[0] := fenwick_add_tree[0] + value;
  exit(makeFenwickTree(f.size, fenwick_add_tree));
end;
  fenwick_add_i := index;
  while fenwick_add_i < f.size do begin
  fenwick_add_tree[fenwick_add_i] := fenwick_add_tree[fenwick_add_i] + value;
  fenwick_add_i := fenwick_next(fenwick_add_i);
end;
  exit(makeFenwickTree(f.size, fenwick_add_tree));
end;
function fenwick_update(f: FenwickTree; index: integer; value: integer): FenwickTree;
var
  fenwick_update_current: integer;
begin
  fenwick_update_current := fenwick_get(f, index);
  exit(fenwick_add(f, index, value - fenwick_update_current));
end;
function fenwick_prefix(f: FenwickTree; right: integer): integer;
var
  fenwick_prefix_result_: integer;
  fenwick_prefix_r: integer;
begin
  if right = 0 then begin
  exit(0);
end;
  fenwick_prefix_result_ := f.tree[0];
  fenwick_prefix_r := right - 1;
  while fenwick_prefix_r > 0 do begin
  fenwick_prefix_result_ := fenwick_prefix_result_ + f.tree[fenwick_prefix_r];
  fenwick_prefix_r := fenwick_prev(fenwick_prefix_r);
end;
  exit(fenwick_prefix_result_);
end;
function fenwick_query(f: FenwickTree; left: integer; right: integer): integer;
begin
  exit(fenwick_prefix(f, right) - fenwick_prefix(f, left));
end;
function fenwick_get(f: FenwickTree; index: integer): integer;
begin
  exit(fenwick_query(f, index, index + 1));
end;
function fenwick_rank_query(f: FenwickTree; value: integer): integer;
var
  fenwick_rank_query_v: integer;
  fenwick_rank_query_j: integer;
  fenwick_rank_query_i: integer;
  fenwick_rank_query_jj: integer;
begin
  fenwick_rank_query_v := value - f.tree[0];
  if fenwick_rank_query_v < 0 then begin
  exit(-1);
end;
  fenwick_rank_query_j := 1;
  while (fenwick_rank_query_j * 2) < f.size do begin
  fenwick_rank_query_j := fenwick_rank_query_j * 2;
end;
  fenwick_rank_query_i := 0;
  fenwick_rank_query_jj := fenwick_rank_query_j;
  while fenwick_rank_query_jj > 0 do begin
  if ((fenwick_rank_query_i + fenwick_rank_query_jj) < f.size) and (f.tree[fenwick_rank_query_i + fenwick_rank_query_jj] <= fenwick_rank_query_v) then begin
  fenwick_rank_query_v := fenwick_rank_query_v - f.tree[fenwick_rank_query_i + fenwick_rank_query_jj];
  fenwick_rank_query_i := fenwick_rank_query_i + fenwick_rank_query_jj;
end;
  fenwick_rank_query_jj := fenwick_rank_query_jj div 2;
end;
  exit(fenwick_rank_query_i);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  f_base := fenwick_from_list([1, 2, 3, 4, 5]);
  show_list(fenwick_get_array(f_base));
  f := fenwick_from_list([1, 2, 3, 4, 5]);
  f := fenwick_add(f, 0, 1);
  f := fenwick_add(f, 1, 2);
  f := fenwick_add(f, 2, 3);
  f := fenwick_add(f, 3, 4);
  f := fenwick_add(f, 4, 5);
  show_list(fenwick_get_array(f));
  f2 := fenwick_from_list([1, 2, 3, 4, 5]);
  writeln(fenwick_prefix(f2, 3));
  writeln(fenwick_query(f2, 1, 4));
  f3 := fenwick_from_list([1, 2, 0, 3, 0, 5]);
  writeln(fenwick_rank_query(f3, 0));
  writeln(fenwick_rank_query(f3, 2));
  writeln(fenwick_rank_query(f3, 1));
  writeln(fenwick_rank_query(f3, 3));
  writeln(fenwick_rank_query(f3, 5));
  writeln(fenwick_rank_query(f3, 6));
  writeln(fenwick_rank_query(f3, 11));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
