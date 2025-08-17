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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  common_diff: integer;
  first_term: integer;
  num_of_terms: integer;
function sum_of_series(first_term: integer; common_diff: integer; num_of_terms: integer): integer; forward;
procedure test_sum_of_series(); forward;
procedure main(); forward;
function sum_of_series(first_term: integer; common_diff: integer; num_of_terms: integer): integer;
var
  sum_of_series_total: integer;
begin
  sum_of_series_total := (num_of_terms * ((2 * first_term) + ((num_of_terms - 1) * common_diff))) div 2;
  exit(sum_of_series_total);
end;
procedure test_sum_of_series();
begin
  if sum_of_series(1, 1, 10) <> 55 then begin
  panic('sum_of_series(1, 1, 10) failed');
end;
  if sum_of_series(1, 10, 100) <> 49600 then begin
  panic('sum_of_series(1, 10, 100) failed');
end;
end;
procedure main();
begin
  test_sum_of_series();
  writeln(sum_of_series(1, 1, 10));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
