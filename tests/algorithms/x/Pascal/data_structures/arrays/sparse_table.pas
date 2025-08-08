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
  st1: IntArrayArray;
  st2: IntArrayArray;
  sparse_table: IntArrayArray;
  right_bound: integer;
  n: integer;
  number_list: IntArray;
  left_bound: integer;
function pow2(n: integer): integer; forward;
function int_log2(n: integer): integer; forward;
function build_sparse_table(number_list: IntArray): IntArrayArray; forward;
function query(sparse_table: IntArrayArray; left_bound: integer; right_bound: integer): integer; forward;
function pow2(n: integer): integer;
var
  pow2_result_: integer;
  pow2_i: integer;
begin
  pow2_result_ := 1;
  pow2_i := 0;
  while pow2_i < n do begin
  pow2_result_ := pow2_result_ * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_result_);
end;
function int_log2(n: integer): integer;
var
  int_log2_v: integer;
  int_log2_res: integer;
begin
  int_log2_v := n;
  int_log2_res := 0;
  while int_log2_v > 1 do begin
  int_log2_v := int_log2_v div 2;
  int_log2_res := int_log2_res + 1;
end;
  exit(int_log2_res);
end;
function build_sparse_table(number_list: IntArray): IntArrayArray;
var
  build_sparse_table_length_: integer;
  build_sparse_table_row: integer;
  build_sparse_table_sparse_table: array of IntArray;
  build_sparse_table_j: integer;
  build_sparse_table_inner: array of integer;
  build_sparse_table_i: integer;
  build_sparse_table_left: integer;
  build_sparse_table_right: integer;
begin
  if Length(number_list) = 0 then begin
  panic('empty number list not allowed');
end;
  build_sparse_table_length_ := Length(number_list);
  build_sparse_table_row := int_log2(build_sparse_table_length_) + 1;
  build_sparse_table_sparse_table := [];
  build_sparse_table_j := 0;
  while build_sparse_table_j < build_sparse_table_row do begin
  build_sparse_table_inner := [];
  build_sparse_table_i := 0;
  while build_sparse_table_i < build_sparse_table_length_ do begin
  build_sparse_table_inner := concat(build_sparse_table_inner, IntArray([0]));
  build_sparse_table_i := build_sparse_table_i + 1;
end;
  build_sparse_table_sparse_table := concat(build_sparse_table_sparse_table, [build_sparse_table_inner]);
  build_sparse_table_j := build_sparse_table_j + 1;
end;
  build_sparse_table_i := 0;
  while build_sparse_table_i < build_sparse_table_length_ do begin
  build_sparse_table_sparse_table[0][build_sparse_table_i] := number_list[build_sparse_table_i];
  build_sparse_table_i := build_sparse_table_i + 1;
end;
  build_sparse_table_j := 1;
  while pow2(build_sparse_table_j) <= build_sparse_table_length_ do begin
  build_sparse_table_i := 0;
  while ((build_sparse_table_i + pow2(build_sparse_table_j)) - 1) < build_sparse_table_length_ do begin
  build_sparse_table_left := build_sparse_table_sparse_table[build_sparse_table_j - 1][build_sparse_table_i + pow2(build_sparse_table_j - 1)];
  build_sparse_table_right := build_sparse_table_sparse_table[build_sparse_table_j - 1][build_sparse_table_i];
  if build_sparse_table_left < build_sparse_table_right then begin
  build_sparse_table_sparse_table[build_sparse_table_j][build_sparse_table_i] := build_sparse_table_left;
end else begin
  build_sparse_table_sparse_table[build_sparse_table_j][build_sparse_table_i] := build_sparse_table_right;
end;
  build_sparse_table_i := build_sparse_table_i + 1;
end;
  build_sparse_table_j := build_sparse_table_j + 1;
end;
  exit(build_sparse_table_sparse_table);
end;
function query(sparse_table: IntArrayArray; left_bound: integer; right_bound: integer): integer;
var
  query_interval: integer;
  query_j: integer;
  query_val1: integer;
  query_val2: integer;
begin
  if (left_bound < 0) or (right_bound >= Length(sparse_table[0])) then begin
  panic('list index out of range');
end;
  query_interval := (right_bound - left_bound) + 1;
  query_j := int_log2(query_interval);
  query_val1 := sparse_table[query_j][(right_bound - pow2(query_j)) + 1];
  query_val2 := sparse_table[query_j][left_bound];
  if query_val1 < query_val2 then begin
  exit(query_val1);
end;
  exit(query_val2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  st1 := build_sparse_table([8, 1, 0, 3, 4, 9, 3]);
  writeln(list_list_int_to_str(st1));
  st2 := build_sparse_table([3, 1, 9]);
  writeln(list_list_int_to_str(st2));
  writeln(IntToStr(query(st1, 0, 4)));
  writeln(IntToStr(query(st1, 4, 6)));
  writeln(IntToStr(query(st2, 2, 2)));
  writeln(IntToStr(query(st2, 0, 1)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
