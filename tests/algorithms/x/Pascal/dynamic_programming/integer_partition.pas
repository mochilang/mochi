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
  m: integer;
function partition(m: integer): integer; forward;
function partition(m: integer): integer;
var
  partition_memo: array of IntArray;
  partition_i: integer;
  partition_row: array of integer;
  partition_j: integer;
  partition_n: integer;
  partition_k: integer;
begin
  partition_memo := [];
  partition_i := 0;
  while partition_i < (m + 1) do begin
  partition_row := [];
  partition_j := 0;
  while partition_j < m do begin
  partition_row := concat(partition_row, IntArray([0]));
  partition_j := partition_j + 1;
end;
  partition_memo := concat(partition_memo, [partition_row]);
  partition_i := partition_i + 1;
end;
  partition_i := 0;
  while partition_i < (m + 1) do begin
  partition_memo[partition_i][0] := 1;
  partition_i := partition_i + 1;
end;
  partition_n := 0;
  while partition_n < (m + 1) do begin
  partition_k := 1;
  while partition_k < m do begin
  partition_memo[partition_n][partition_k] := partition_memo[partition_n][partition_k] + partition_memo[partition_n][partition_k - 1];
  if (partition_n - partition_k) > 0 then begin
  partition_memo[partition_n][partition_k] := partition_memo[partition_n][partition_k] + partition_memo[(partition_n - partition_k) - 1][partition_k];
end;
  partition_k := partition_k + 1;
end;
  partition_n := partition_n + 1;
end;
  exit(partition_memo[m][m - 1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(partition(5));
  writeln(partition(7));
  writeln(partition(100));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
