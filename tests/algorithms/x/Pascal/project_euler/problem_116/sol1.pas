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
  length_: integer;
function solution(length_: integer): integer; forward;
function solution(length_: integer): integer;
var
  solution_ways: array of IntArray;
  solution_i: integer;
  solution_row: array of integer;
  solution_row_length: integer;
  solution_tile_length: integer;
  solution_tile_start: integer;
  solution_remaining: integer;
  solution_total: integer;
  solution_j: integer;
begin
  solution_ways := [];
  solution_i := 0;
  while solution_i <= length_ do begin
  solution_row := [];
  solution_row := concat(solution_row, IntArray([0]));
  solution_row := concat(solution_row, IntArray([0]));
  solution_row := concat(solution_row, IntArray([0]));
  solution_ways := concat(solution_ways, [solution_row]);
  solution_i := solution_i + 1;
end;
  solution_row_length := 0;
  while solution_row_length <= length_ do begin
  solution_tile_length := 2;
  while solution_tile_length <= 4 do begin
  solution_tile_start := 0;
  while solution_tile_start <= (solution_row_length - solution_tile_length) do begin
  solution_remaining := (solution_row_length - solution_tile_start) - solution_tile_length;
  solution_ways[solution_row_length][solution_tile_length - 2] := (solution_ways[solution_row_length][solution_tile_length - 2] + solution_ways[solution_remaining][solution_tile_length - 2]) + 1;
  solution_tile_start := solution_tile_start + 1;
end;
  solution_tile_length := solution_tile_length + 1;
end;
  solution_row_length := solution_row_length + 1;
end;
  solution_total := 0;
  solution_j := 0;
  while solution_j < 3 do begin
  solution_total := solution_total + solution_ways[length_][solution_j];
  solution_j := solution_j + 1;
end;
  exit(solution_total);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(solution(5));
  writeln(solution(50));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
