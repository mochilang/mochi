{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Query = record
  left: int64;
  right: int64;
end;
type IntArray = array of int64;
type QueryArray = array of Query;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64);
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
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
  arr1: array of int64;
  queries1: array of Query;
  arr2: array of int64;
  queries2: array of Query;
function makeQuery(left: int64; right: int64): Query; forward;
function prefix_sum(prefix_sum_arr: IntArray; prefix_sum_queries: QueryArray): IntArray; forward;
function makeQuery(left: int64; right: int64): Query;
begin
  Result.left := left;
  Result.right := right;
end;
function prefix_sum(prefix_sum_arr: IntArray; prefix_sum_queries: QueryArray): IntArray;
var
  prefix_sum_dp: array of int64;
  prefix_sum_i: int64;
  prefix_sum_result_: array of int64;
  prefix_sum_j: int64;
  prefix_sum_q: Query;
  prefix_sum_sum: int64;
begin
  prefix_sum_dp := [];
  prefix_sum_i := 0;
  while prefix_sum_i < Length(prefix_sum_arr) do begin
  if prefix_sum_i = 0 then begin
  prefix_sum_dp := concat(prefix_sum_dp, IntArray([prefix_sum_arr[0]]));
end else begin
  prefix_sum_dp := concat(prefix_sum_dp, IntArray([prefix_sum_dp[prefix_sum_i - 1] + prefix_sum_arr[prefix_sum_i]]));
end;
  prefix_sum_i := prefix_sum_i + 1;
end;
  prefix_sum_result_ := [];
  prefix_sum_j := 0;
  while prefix_sum_j < Length(prefix_sum_queries) do begin
  prefix_sum_q := prefix_sum_queries[prefix_sum_j];
  prefix_sum_sum := prefix_sum_dp[prefix_sum_q.right];
  if prefix_sum_q.left > 0 then begin
  prefix_sum_sum := prefix_sum_sum - prefix_sum_dp[prefix_sum_q.left - 1];
end;
  prefix_sum_result_ := concat(prefix_sum_result_, IntArray([prefix_sum_sum]));
  prefix_sum_j := prefix_sum_j + 1;
end;
  exit(prefix_sum_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  arr1 := [1, 4, 6, 2, 61, 12];
  queries1 := [makeQuery(2, 5), makeQuery(1, 5), makeQuery(3, 4)];
  writeln(list_int_to_str(prefix_sum(arr1, queries1)));
  arr2 := [4, 2, 1, 6, 3];
  queries2 := [makeQuery(3, 4), makeQuery(1, 3), makeQuery(0, 2)];
  writeln(list_int_to_str(prefix_sum(arr2, queries2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
