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
  x: real;
  beta: real;
  b: RealArray;
  vector: RealArray;
  eps: real;
  a: RealArray;
function exp_approx(x: real): real; forward;
function sigmoid(vector: RealArray): RealArray; forward;
function swish(vector: RealArray; beta: real): RealArray; forward;
function sigmoid_linear_unit(vector: RealArray): RealArray; forward;
function approx_equal(a: real; b: real; eps: real): boolean; forward;
function approx_equal_list(a: RealArray; b: RealArray; eps: real): boolean; forward;
procedure test_swish(); forward;
procedure main(); forward;
function exp_approx(x: real): real;
var
  exp_approx_sum: real;
  exp_approx_term: real;
  exp_approx_i: integer;
begin
  exp_approx_sum := 1;
  exp_approx_term := 1;
  exp_approx_i := 1;
  while exp_approx_i <= 20 do begin
  exp_approx_term := (exp_approx_term * x) / Double(exp_approx_i);
  exp_approx_sum := exp_approx_sum + exp_approx_term;
  exp_approx_i := exp_approx_i + 1;
end;
  exit(exp_approx_sum);
end;
function sigmoid(vector: RealArray): RealArray;
var
  sigmoid_result_: array of real;
  sigmoid_i: integer;
  sigmoid_v: real;
  sigmoid_s: real;
begin
  sigmoid_result_ := [];
  sigmoid_i := 0;
  while sigmoid_i < Length(vector) do begin
  sigmoid_v := vector[sigmoid_i];
  sigmoid_s := 1 / (1 + exp_approx(-sigmoid_v));
  sigmoid_result_ := concat(sigmoid_result_, [sigmoid_s]);
  sigmoid_i := sigmoid_i + 1;
end;
  exit(sigmoid_result_);
end;
function swish(vector: RealArray; beta: real): RealArray;
var
  swish_result_: array of real;
  swish_i: integer;
  swish_v: real;
  swish_s: real;
begin
  swish_result_ := [];
  swish_i := 0;
  while swish_i < Length(vector) do begin
  swish_v := vector[swish_i];
  swish_s := 1 / (1 + exp_approx(-beta * swish_v));
  swish_result_ := concat(swish_result_, [swish_v * swish_s]);
  swish_i := swish_i + 1;
end;
  exit(swish_result_);
end;
function sigmoid_linear_unit(vector: RealArray): RealArray;
begin
  exit(swish(vector, 1));
end;
function approx_equal(a: real; b: real; eps: real): boolean;
var
  approx_equal_diff: real;
begin
  if a > b then begin
  approx_equal_diff := a - b;
end else begin
  approx_equal_diff := b - a;
end;
  exit(approx_equal_diff < eps);
end;
function approx_equal_list(a: RealArray; b: RealArray; eps: real): boolean;
var
  approx_equal_list_i: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  approx_equal_list_i := 0;
  while approx_equal_list_i < Length(a) do begin
  if not approx_equal(a[approx_equal_list_i], b[approx_equal_list_i], eps) then begin
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
end.
