{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function sum_of_harmonic_progression(sum_of_harmonic_progression_first_term: real; sum_of_harmonic_progression_common_difference: real; sum_of_harmonic_progression_number_of_terms: int64): real; forward;
function abs_val(abs_val_num: real): real; forward;
procedure test_sum_of_harmonic_progression(); forward;
procedure main(); forward;
function sum_of_harmonic_progression(sum_of_harmonic_progression_first_term: real; sum_of_harmonic_progression_common_difference: real; sum_of_harmonic_progression_number_of_terms: int64): real;
var
  sum_of_harmonic_progression_arithmetic_progression: array of real;
  sum_of_harmonic_progression_term: real;
  sum_of_harmonic_progression_i: int64;
  sum_of_harmonic_progression_total: real;
  sum_of_harmonic_progression_j: int64;
begin
  sum_of_harmonic_progression_arithmetic_progression := [1 / sum_of_harmonic_progression_first_term];
  sum_of_harmonic_progression_term := 1 / sum_of_harmonic_progression_first_term;
  sum_of_harmonic_progression_i := 0;
  while sum_of_harmonic_progression_i < (sum_of_harmonic_progression_number_of_terms - 1) do begin
  sum_of_harmonic_progression_term := sum_of_harmonic_progression_term + sum_of_harmonic_progression_common_difference;
  sum_of_harmonic_progression_arithmetic_progression := concat(sum_of_harmonic_progression_arithmetic_progression, [sum_of_harmonic_progression_term]);
  sum_of_harmonic_progression_i := sum_of_harmonic_progression_i + 1;
end;
  sum_of_harmonic_progression_total := 0;
  sum_of_harmonic_progression_j := 0;
  while sum_of_harmonic_progression_j < Length(sum_of_harmonic_progression_arithmetic_progression) do begin
  sum_of_harmonic_progression_total := sum_of_harmonic_progression_total + (1 / sum_of_harmonic_progression_arithmetic_progression[sum_of_harmonic_progression_j]);
  sum_of_harmonic_progression_j := sum_of_harmonic_progression_j + 1;
end;
  exit(sum_of_harmonic_progression_total);
end;
function abs_val(abs_val_num: real): real;
begin
  if abs_val_num < 0 then begin
  exit(-abs_val_num);
end;
  exit(abs_val_num);
end;
procedure test_sum_of_harmonic_progression();
var
  test_sum_of_harmonic_progression_result1: real;
  test_sum_of_harmonic_progression_result2: real;
begin
  test_sum_of_harmonic_progression_result1 := sum_of_harmonic_progression(0.5, 2, 2);
  if abs_val(test_sum_of_harmonic_progression_result1 - 0.75) > 1e-07 then begin
  panic('test1 failed');
end;
  test_sum_of_harmonic_progression_result2 := sum_of_harmonic_progression(0.2, 5, 5);
  if abs_val(test_sum_of_harmonic_progression_result2 - 0.45666666666666667) > 1e-07 then begin
  panic('test2 failed');
end;
end;
procedure main();
begin
  test_sum_of_harmonic_progression();
  writeln(sum_of_harmonic_progression(0.5, 2, 2));
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
  writeln('');
end.
