{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
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
  x: array of array of real;
  y: array of real;
  alpha: real;
  iterations: integer;
  theta: RealArray;
  i: int64;
  b: RealArray;
  n: integer;
  z: real;
  a: RealArray;
function expApprox(x: real): real; forward;
function sigmoid(z: real): real; forward;
function dot(a: RealArray; b: RealArray): real; forward;
function zeros(n: integer): RealArray; forward;
function logistic_reg(alpha: real; x: RealArrayArray; y: RealArray; iterations: integer): RealArray; forward;
function expApprox(x: real): real;
var
  expApprox_y: real;
  expApprox_is_neg: boolean;
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: integer;
begin
  expApprox_y := x;
  expApprox_is_neg := false;
  if x < 0 then begin
  expApprox_is_neg := true;
  expApprox_y := -x;
end;
  expApprox_term := 1;
  expApprox_sum := 1;
  expApprox_n := 1;
  while expApprox_n < 30 do begin
  expApprox_term := (expApprox_term * expApprox_y) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  if expApprox_is_neg then begin
  exit(1 / expApprox_sum);
end;
  exit(expApprox_sum);
end;
function sigmoid(z: real): real;
begin
  exit(1 / (1 + expApprox(-z)));
end;
function dot(a: RealArray; b: RealArray): real;
var
  dot_s: real;
  dot_i: integer;
begin
  dot_s := 0;
  dot_i := 0;
  while dot_i < Length(a) do begin
  dot_s := dot_s + (a[dot_i] * b[dot_i]);
  dot_i := dot_i + 1;
end;
  exit(dot_s);
end;
function zeros(n: integer): RealArray;
var
  zeros_res: array of real;
  zeros_i: integer;
begin
  zeros_res := [];
  zeros_i := 0;
  while zeros_i < n do begin
  zeros_res := concat(zeros_res, [0]);
  zeros_i := zeros_i + 1;
end;
  exit(zeros_res);
end;
function logistic_reg(alpha: real; x: RealArrayArray; y: RealArray; iterations: integer): RealArray;
var
  logistic_reg_m: integer;
  logistic_reg_n: integer;
  logistic_reg_theta: RealArray;
  logistic_reg_iter: integer;
  logistic_reg_grad: RealArray;
  logistic_reg_i: integer;
  logistic_reg_z: real;
  logistic_reg_h: real;
  logistic_reg_k: integer;
  logistic_reg_k2: integer;
begin
  logistic_reg_m := Length(x);
  logistic_reg_n := Length(x[0]);
  logistic_reg_theta := zeros(logistic_reg_n);
  logistic_reg_iter := 0;
  while logistic_reg_iter < iterations do begin
  logistic_reg_grad := zeros(logistic_reg_n);
  logistic_reg_i := 0;
  while logistic_reg_i < logistic_reg_m do begin
  logistic_reg_z := dot(x[logistic_reg_i], logistic_reg_theta);
  logistic_reg_h := sigmoid(logistic_reg_z);
  logistic_reg_k := 0;
  while logistic_reg_k < logistic_reg_n do begin
  logistic_reg_grad[logistic_reg_k] := logistic_reg_grad[logistic_reg_k] + ((logistic_reg_h - y[logistic_reg_i]) * x[logistic_reg_i][logistic_reg_k]);
  logistic_reg_k := logistic_reg_k + 1;
end;
  logistic_reg_i := logistic_reg_i + 1;
end;
  logistic_reg_k2 := 0;
  while logistic_reg_k2 < logistic_reg_n do begin
  logistic_reg_theta[logistic_reg_k2] := logistic_reg_theta[logistic_reg_k2] - ((alpha * logistic_reg_grad[logistic_reg_k2]) / Double(logistic_reg_m));
  logistic_reg_k2 := logistic_reg_k2 + 1;
end;
  logistic_reg_iter := logistic_reg_iter + 1;
end;
  exit(logistic_reg_theta);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  x := [[0.5, 1.5], [1, 1], [1.5, 0.5], [3, 3.5], [3.5, 3], [4, 4]];
  y := [0, 0, 0, 1, 1, 1];
  alpha := 0.1;
  iterations := 1000;
  theta := logistic_reg(alpha, x, y, iterations);
  for i := 0 to (Length(theta) - 1) do begin
  writeln(theta[i]);
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
