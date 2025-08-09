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
  INF: integer;
  arr: IntArray;
function matrix_chain_multiply(arr: IntArray): integer; forward;
function matrix_chain_multiply(arr: IntArray): integer;
var
  matrix_chain_multiply_n: integer;
  matrix_chain_multiply_dp: array of IntArray;
  matrix_chain_multiply_i: integer;
  matrix_chain_multiply_row: array of integer;
  matrix_chain_multiply_j: integer;
  matrix_chain_multiply_k: integer;
  matrix_chain_multiply_cost: integer;
begin
  if Length(arr) < 2 then begin
  exit(0);
end;
  matrix_chain_multiply_n := Length(arr);
  matrix_chain_multiply_dp := [];
  matrix_chain_multiply_i := 0;
  while matrix_chain_multiply_i < matrix_chain_multiply_n do begin
  matrix_chain_multiply_row := [];
  matrix_chain_multiply_j := 0;
  while matrix_chain_multiply_j < matrix_chain_multiply_n do begin
  matrix_chain_multiply_row := concat(matrix_chain_multiply_row, IntArray([INF]));
  matrix_chain_multiply_j := matrix_chain_multiply_j + 1;
end;
  matrix_chain_multiply_dp := concat(matrix_chain_multiply_dp, [matrix_chain_multiply_row]);
  matrix_chain_multiply_i := matrix_chain_multiply_i + 1;
end;
  matrix_chain_multiply_i := matrix_chain_multiply_n - 1;
  while matrix_chain_multiply_i > 0 do begin
  matrix_chain_multiply_j := matrix_chain_multiply_i;
  while matrix_chain_multiply_j < matrix_chain_multiply_n do begin
  if matrix_chain_multiply_i = matrix_chain_multiply_j then begin
  matrix_chain_multiply_dp[matrix_chain_multiply_i][matrix_chain_multiply_j] := 0;
end else begin
  matrix_chain_multiply_k := matrix_chain_multiply_i;
  while matrix_chain_multiply_k < matrix_chain_multiply_j do begin
  matrix_chain_multiply_cost := (matrix_chain_multiply_dp[matrix_chain_multiply_i][matrix_chain_multiply_k] + matrix_chain_multiply_dp[matrix_chain_multiply_k + 1][matrix_chain_multiply_j]) + ((arr[matrix_chain_multiply_i - 1] * arr[matrix_chain_multiply_k]) * arr[matrix_chain_multiply_j]);
  if matrix_chain_multiply_cost < matrix_chain_multiply_dp[matrix_chain_multiply_i][matrix_chain_multiply_j] then begin
  matrix_chain_multiply_dp[matrix_chain_multiply_i][matrix_chain_multiply_j] := matrix_chain_multiply_cost;
end;
  matrix_chain_multiply_k := matrix_chain_multiply_k + 1;
end;
end;
  matrix_chain_multiply_j := matrix_chain_multiply_j + 1;
end;
  matrix_chain_multiply_i := matrix_chain_multiply_i - 1;
end;
  exit(matrix_chain_multiply_dp[1][matrix_chain_multiply_n - 1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  INF := 1000000000;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
