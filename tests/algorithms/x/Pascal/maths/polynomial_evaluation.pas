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
  base: real;
  exponent: integer;
  poly: RealArray;
  x: real;
function pow_float(base: real; exponent: integer): real; forward;
function evaluate_poly(poly: RealArray; x: real): real; forward;
function horner(poly: RealArray; x: real): real; forward;
procedure test_polynomial_evaluation(); forward;
procedure main(); forward;
function pow_float(base: real; exponent: integer): real;
var
  pow_float_exp_: integer;
  pow_float_result_: real;
  pow_float_i: integer;
begin
  pow_float_exp_ := exponent;
  pow_float_result_ := 1;
  pow_float_i := 0;
  while pow_float_i < pow_float_exp_ do begin
  pow_float_result_ := pow_float_result_ * base;
  pow_float_i := pow_float_i + 1;
end;
  exit(pow_float_result_);
end;
function evaluate_poly(poly: RealArray; x: real): real;
var
  evaluate_poly_total: real;
  evaluate_poly_i: integer;
begin
  evaluate_poly_total := 0;
  evaluate_poly_i := 0;
  while evaluate_poly_i < Length(poly) do begin
  evaluate_poly_total := evaluate_poly_total + (poly[evaluate_poly_i] * pow_float(x, evaluate_poly_i));
  evaluate_poly_i := evaluate_poly_i + 1;
end;
  exit(evaluate_poly_total);
end;
function horner(poly: RealArray; x: real): real;
var
  horner_result_: real;
  horner_i: integer;
begin
  horner_result_ := 0;
  horner_i := Length(poly) - 1;
  while horner_i >= 0 do begin
  horner_result_ := (horner_result_ * x) + poly[horner_i];
  horner_i := horner_i - 1;
end;
  exit(horner_result_);
end;
procedure test_polynomial_evaluation();
var
  test_polynomial_evaluation_poly: array of real;
  test_polynomial_evaluation_x: real;
begin
  test_polynomial_evaluation_poly := [0, 0, 5, 9.3, 7];
  test_polynomial_evaluation_x := 10;
  if evaluate_poly(test_polynomial_evaluation_poly, test_polynomial_evaluation_x) <> 79800 then begin
  panic('evaluate_poly failed');
end;
  if horner(test_polynomial_evaluation_poly, test_polynomial_evaluation_x) <> 79800 then begin
  panic('horner failed');
end;
end;
procedure main();
var
  main_poly: array of real;
  main_x: real;
begin
  test_polynomial_evaluation();
  main_poly := [0, 0, 5, 9.3, 7];
  main_x := 10;
  writeln(evaluate_poly(main_poly, main_x));
  writeln(horner(main_poly, main_x));
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
