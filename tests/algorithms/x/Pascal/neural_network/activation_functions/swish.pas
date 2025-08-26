{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function exp_approx(exp_approx_x: real): real; forward;
function sigmoid(sigmoid_vector: RealArray): RealArray; forward;
function swish(swish_vector: RealArray; swish_beta: real): RealArray; forward;
function sigmoid_linear_unit(sigmoid_linear_unit_vector: RealArray): RealArray; forward;
function approx_equal(approx_equal_a: real; approx_equal_b: real; approx_equal_eps: real): boolean; forward;
function approx_equal_list(approx_equal_list_a: RealArray; approx_equal_list_b: RealArray; approx_equal_list_eps: real): boolean; forward;
procedure test_swish(); forward;
procedure main(); forward;
function exp_approx(exp_approx_x: real): real;
var
  exp_approx_sum: real;
  exp_approx_term: real;
  exp_approx_i: int64;
begin
  exp_approx_sum := 1;
  exp_approx_term := 1;
  exp_approx_i := 1;
  while exp_approx_i <= 20 do begin
  exp_approx_term := (exp_approx_term * exp_approx_x) / Double(exp_approx_i);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  exit(exp_approx_sum);
end;
function sigmoid(sigmoid_vector: RealArray): RealArray;
var
  sigmoid_result_: array of real;
  sigmoid_i: int64;
  sigmoid_v: real;
  sigmoid_s: real;
begin
  sigmoid_result_ := [];
  sigmoid_i := 0;
  while sigmoid_i < Length(sigmoid_vector) do begin
  sigmoid_v := sigmoid_vector[sigmoid_i];
  sigmoid_s := 1 / (1 + exp_approx(-sigmoid_v));
  sigmoid_result_ := concat(sigmoid_result_, [sigmoid_s]);
  sigmoid_i := sigmoid_i + 1;
end;
  exit(sigmoid_result_);
end;
function swish(swish_vector: RealArray; swish_beta: real): RealArray;
var
  swish_result_: array of real;
  swish_i: int64;
  swish_v: real;
  swish_s: real;
begin
  swish_result_ := [];
  swish_i := 0;
  while swish_i < Length(swish_vector) do begin
  swish_v := swish_vector[swish_i];
  swish_s := 1 / (1 + exp_approx(-swish_beta * swish_v));
  swish_result_ := concat(swish_result_, [swish_v * swish_s]);
  swish_i := swish_i + 1;
end;
  exit(swish_result_);
end;
function sigmoid_linear_unit(sigmoid_linear_unit_vector: RealArray): RealArray;
begin
  exit(swish(sigmoid_linear_unit_vector, 1));
end;
function approx_equal(approx_equal_a: real; approx_equal_b: real; approx_equal_eps: real): boolean;
var
  approx_equal_diff: real;
begin
  if approx_equal_a > approx_equal_b then begin
  approx_equal_diff := approx_equal_a - approx_equal_b;
end else begin
  approx_equal_diff := approx_equal_b - approx_equal_a;
end;
  exit(approx_equal_diff < approx_equal_eps);
end;
function approx_equal_list(approx_equal_list_a: RealArray; approx_equal_list_b: RealArray; approx_equal_list_eps: real): boolean;
var
  approx_equal_list_i: int64;
begin
  if Length(approx_equal_list_a) <> Length(approx_equal_list_b) then begin
  exit(false);
end;
  approx_equal_list_i := 0;
  while approx_equal_list_i < Length(approx_equal_list_a) do begin
  if not approx_equal(approx_equal_list_a[approx_equal_list_i], approx_equal_list_b[approx_equal_list_i], approx_equal_list_eps) then begin
  exit(false);
end;
  approx_equal_list_i := approx_equal_list_i + 1;
end;
  exit(true);
end;
procedure test_swish();
var
  test_swish_v: array of real;
  test_swish_eps: real;
begin
  test_swish_v := [-1, 1, 2];
  test_swish_eps := 0.001;
  if not approx_equal_list(sigmoid(test_swish_v), [0.26894142, 0.73105858, 0.88079708], test_swish_eps) then begin
  panic('sigmoid incorrect');
end;
  if not approx_equal_list(sigmoid_linear_unit(test_swish_v), [-0.26894142, 0.73105858, 1.76159416], test_swish_eps) then begin
  panic('sigmoid_linear_unit incorrect');
end;
  if not approx_equal_list(swish(test_swish_v, 2), [-0.11920292, 0.88079708, 1.96402758], test_swish_eps) then begin
  panic('swish incorrect');
end;
  if not approx_equal_list(swish([-2], 1), [-0.23840584], test_swish_eps) then begin
  panic('swish with parameter 1 incorrect');
end;
end;
procedure main();
begin
  test_swish();
  writeln(list_real_to_str(sigmoid([-1, 1, 2])));
  writeln(list_real_to_str(sigmoid_linear_unit([-1, 1, 2])));
  writeln(list_real_to_str(swish([-1, 1, 2], 2)));
  writeln(list_real_to_str(swish([-2], 1)));
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
