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
function pow_float(pow_float_base: real; pow_float_exp_: integer): real; forward;
function sum_of_geometric_progression(sum_of_geometric_progression_first_term: integer; sum_of_geometric_progression_common_ratio: integer; sum_of_geometric_progression_num_of_terms: integer): real; forward;
procedure test_sum(); forward;
procedure main(); forward;
function pow_float(pow_float_base: real; pow_float_exp_: integer): real;
var
  pow_float_result_: real;
  pow_float_exponent: integer;
  pow_float_i: integer;
  pow_float_i_7: integer;
begin
  pow_float_result_ := 1;
  pow_float_exponent := pow_float_exp_;
  if pow_float_exponent < 0 then begin
  pow_float_exponent := -pow_float_exponent;
  pow_float_i := 0;
  while pow_float_i < pow_float_exponent do begin
  pow_float_result_ := pow_float_result_ * pow_float_base;
  pow_float_i := pow_float_i + 1;
end;
  exit(1 / pow_float_result_);
end;
  pow_float_i_7 := 0;
  while pow_float_i_7 < pow_float_exponent do begin
  pow_float_result_ := pow_float_result_ * pow_float_base;
  pow_float_i_7 := pow_float_i_7 + 1;
end;
  exit(pow_float_result_);
end;
function sum_of_geometric_progression(sum_of_geometric_progression_first_term: integer; sum_of_geometric_progression_common_ratio: integer; sum_of_geometric_progression_num_of_terms: integer): real;
var
  sum_of_geometric_progression_a: real;
  sum_of_geometric_progression_r: real;
begin
  if sum_of_geometric_progression_common_ratio = 1 then begin
  exit(Double(sum_of_geometric_progression_num_of_terms * sum_of_geometric_progression_first_term));
end;
  sum_of_geometric_progression_a := Double(sum_of_geometric_progression_first_term);
  sum_of_geometric_progression_r := Double(sum_of_geometric_progression_common_ratio);
  exit((sum_of_geometric_progression_a / (1 - sum_of_geometric_progression_r)) * (1 - pow_float(sum_of_geometric_progression_r, sum_of_geometric_progression_num_of_terms)));
end;
procedure test_sum();
begin
  if sum_of_geometric_progression(1, 2, 10) <> 1023 then begin
  panic('example1 failed');
end;
  if sum_of_geometric_progression(1, 10, 5) <> 11111 then begin
  panic('example2 failed');
end;
  if sum_of_geometric_progression(-1, 2, 10) <> -1023 then begin
  panic('example3 failed');
end;
end;
procedure main();
begin
  test_sum();
  writeln(sum_of_geometric_progression(1, 2, 10));
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
