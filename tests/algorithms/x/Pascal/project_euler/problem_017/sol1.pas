{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function solution(solution_n: int64): int64; forward;
function solution(solution_n: int64): int64;
var
  solution_ones_counts: array of int64;
  solution_tens_counts: array of int64;
  solution_count: int64;
  solution_i: int64;
  solution_remainder: int64;
begin
  solution_ones_counts := [0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8];
  solution_tens_counts := [0, 0, 6, 6, 5, 5, 5, 7, 6, 6];
  solution_count := 0;
  solution_i := 1;
  while solution_i <= solution_n do begin
  if solution_i < 1000 then begin
  if solution_i >= 100 then begin
  solution_count := (solution_count + solution_ones_counts[solution_i div 100]) + 7;
  if (solution_i mod 100) <> 0 then begin
  solution_count := solution_count + 3;
end;
end;
  solution_remainder := solution_i mod 100;
  if (solution_remainder > 0) and (solution_remainder < 20) then begin
  solution_count := solution_count + solution_ones_counts[solution_remainder];
end else begin
  solution_count := solution_count + solution_ones_counts[solution_i mod 10];
  solution_count := solution_count + solution_tens_counts[(solution_remainder - (solution_i mod 10)) div 10];
end;
end else begin
  solution_count := (solution_count + solution_ones_counts[solution_i div 1000]) + 8;
end;
  solution_i := solution_i + 1;
end;
  exit(solution_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution(1000)));
  writeln(IntToStr(solution(5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
