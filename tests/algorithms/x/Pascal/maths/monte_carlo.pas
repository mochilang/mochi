{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type FuncType1 = function(arg0: real): real is nested;
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
  PI: real;
  rand_seed: integer;
  semi_circle_y: real;
  semi_circle_s: real;
  f: FuncType1;
  iterations: integer;
  max_val: real;
  max_value: real;
  min_val: real;
  min_value: real;
  x: real;
function rand_float(): real; forward;
function rand_range(min_val: real; max_val: real): real; forward;
function abs_float(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
procedure pi_estimator(iterations: integer); forward;
function area_under_curve_estimator(iterations: integer; f: FuncType1; min_value: real; max_value: real): real; forward;
procedure area_under_line_estimator_check(iterations: integer; min_value: real; max_value: real); forward;
procedure pi_estimator_using_area_under_curve(iterations: integer); forward;
procedure main(); forward;
function rand_float(): real;
begin
  rand_seed := ((1103515245 * rand_seed) + 12345) mod 2147483648;
  exit(Double(rand_seed) / 2.147483648e+09);
end;
function rand_range(min_val: real; max_val: real): real;
begin
  exit((rand_float() * (max_val - min_val)) + min_val);
end;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x = 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
procedure pi_estimator(iterations: integer);
var
  pi_estimator_inside: real;
  pi_estimator_i: integer;
  pi_estimator_x: real;
  pi_estimator_y: real;
  pi_estimator_proportion: real;
  pi_estimator_pi_estimate: real;
begin
  pi_estimator_inside := 0;
  pi_estimator_i := 0;
  while pi_estimator_i < iterations do begin
  pi_estimator_x := rand_range(-1, 1);
  pi_estimator_y := rand_range(-1, 1);
  if ((pi_estimator_x * pi_estimator_x) + (pi_estimator_y * pi_estimator_y)) <= 1 then begin
  pi_estimator_inside := pi_estimator_inside + 1;
end;
  pi_estimator_i := pi_estimator_i + 1;
end;
  pi_estimator_proportion := pi_estimator_inside / Double(iterations);
  pi_estimator_pi_estimate := pi_estimator_proportion * 4;
  writeln('The estimated value of pi is', ' ', pi_estimator_pi_estimate);
  writeln('The numpy value of pi is', ' ', PI);
  writeln('The total error is', ' ', abs_float(PI - pi_estimator_pi_estimate));
end;
function area_under_curve_estimator(iterations: integer; f: FuncType1; min_value: real; max_value: real): real;
var
  area_under_curve_estimator_sum: real;
  area_under_curve_estimator_i: integer;
  area_under_curve_estimator_x: real;
  area_under_curve_estimator_expected: real;
begin
  area_under_curve_estimator_sum := 0;
  area_under_curve_estimator_i := 0;
  while area_under_curve_estimator_i < iterations do begin
  area_under_curve_estimator_x := rand_range(min_value, max_value);
  area_under_curve_estimator_sum := area_under_curve_estimator_sum + f(area_under_curve_estimator_x);
  area_under_curve_estimator_i := area_under_curve_estimator_i + 1;
end;
  area_under_curve_estimator_expected := area_under_curve_estimator_sum / Double(iterations);
  exit(area_under_curve_estimator_expected * (max_value - min_value));
end;
procedure area_under_line_estimator_check(iterations: integer; min_value: real; max_value: real);
var
  area_under_line_estimator_check_estimated_value: real;
  area_under_line_estimator_check_expected_value: real;
  function identity_function(x: real): real;
begin
  exit(x);
end;
begin
  area_under_line_estimator_check_estimated_value := area_under_curve_estimator(iterations, @identity_function, min_value, max_value);
  area_under_line_estimator_check_expected_value := ((max_value * max_value) - (min_value * min_value)) / 2;
  writeln('******************');
  writeln('Estimating area under y=x where x varies from', ' ', min_value);
  writeln('Estimated value is', ' ', area_under_line_estimator_check_estimated_value);
  writeln('Expected value is', ' ', area_under_line_estimator_check_expected_value);
  writeln('Total error is', ' ', abs_float(area_under_line_estimator_check_estimated_value - area_under_line_estimator_check_expected_value));
  writeln('******************');
end;
procedure pi_estimator_using_area_under_curve(iterations: integer);
var
  pi_estimator_using_area_under_curve_estimated_value: real;
  function semi_circle(x: real): real;
begin
  semi_circle_y := 4 - (x * x);
  semi_circle_s := sqrtApprox(semi_circle_y);
  exit(semi_circle_s);
end;
begin
  pi_estimator_using_area_under_curve_estimated_value := area_under_curve_estimator(iterations, @semi_circle, 0, 2);
  writeln('******************');
  writeln('Estimating pi using area_under_curve_estimator');
  writeln('Estimated value is', ' ', pi_estimator_using_area_under_curve_estimated_value);
  writeln('Expected value is', ' ', PI);
  writeln('Total error is', ' ', abs_float(pi_estimator_using_area_under_curve_estimated_value - PI));
  writeln('******************');
end;
procedure main();
begin
  pi_estimator(1000);
  area_under_line_estimator_check(1000, 0, 1);
  pi_estimator_using_area_under_curve(1000);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  rand_seed := 123456789;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
