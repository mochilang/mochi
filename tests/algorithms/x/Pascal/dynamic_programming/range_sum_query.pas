{$mode objfpc}
program Main;
uses SysUtils;
type Query = record
  left: integer;
  right: integer;
end;
type QueryArray = array of Query;
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
  arr1: array of integer;
  queries1: array of Query;
  arr2: array of integer;
  queries2: array of Query;
  queries: QueryArray;
  arr: IntArray;
function makeQuery(left: integer; right: integer): Query; forward;
function prefix_sum(arr: IntArray; queries: QueryArray): IntArray; forward;
function makeQuery(left: integer; right: integer): Query;
begin
  Result.left := left;
  Result.right := right;
end;
function prefix_sum(arr: IntArray; queries: QueryArray): IntArray;
var
  prefix_sum_dp: array of integer;
  prefix_sum_i: integer;
  prefix_sum_result_: array of integer;
  prefix_sum_j: integer;
  prefix_sum_q: Query;
  prefix_sum_sum: integer;
begin
  prefix_sum_dp := [];
  prefix_sum_i := 0;
  while prefix_sum_i < Length(arr) do begin
  if prefix_sum_i = 0 then begin
  prefix_sum_dp := concat(prefix_sum_dp, IntArray([arr[0]]));
end else begin
  prefix_sum_dp := concat(prefix_sum_dp, IntArray([prefix_sum_dp[prefix_sum_i - 1] + arr[prefix_sum_i]]));
end;
  prefix_sum_i := prefix_sum_i + 1;
end;
  prefix_sum_result_ := [];
  prefix_sum_j := 0;
  while prefix_sum_j < Length(queries) do begin
  prefix_sum_q := queries[prefix_sum_j];
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
end.
