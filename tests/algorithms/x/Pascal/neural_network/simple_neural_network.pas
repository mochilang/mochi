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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: int64;
  INITIAL_VALUE: real;
  result_: real;
function rand(): int64; forward;
function randint(randint_low: int64; randint_high: int64): int64; forward;
function expApprox(expApprox_x: real): real; forward;
function sigmoid(sigmoid_x: real): real; forward;
function sigmoid_derivative(sigmoid_derivative_sig_val: real): real; forward;
function forward_propagation(forward_propagation_expected: int64; forward_propagation_number_propagations: int64): real; forward;
function rand(): int64;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed);
end;
function randint(randint_low: int64; randint_high: int64): int64;
begin
  exit((rand() mod ((randint_high - randint_low) + 1)) + randint_low);
end;
function expApprox(expApprox_x: real): real;
var
  expApprox_y: real;
  expApprox_is_neg: boolean;
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: int64;
begin
  expApprox_y := expApprox_x;
  expApprox_is_neg := false;
  if expApprox_x < 0 then begin
  expApprox_is_neg := true;
  expApprox_y := -expApprox_x;
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
function sigmoid(sigmoid_x: real): real;
begin
  exit(1 / (1 + expApprox(-sigmoid_x)));
end;
function sigmoid_derivative(sigmoid_derivative_sig_val: real): real;
begin
  exit(sigmoid_derivative_sig_val * (1 - sigmoid_derivative_sig_val));
end;
function forward_propagation(forward_propagation_expected: int64; forward_propagation_number_propagations: int64): real;
var
  forward_propagation_weight: real;
  forward_propagation_layer_1: real;
  forward_propagation_i: int64;
  forward_propagation_layer_1_error: real;
  forward_propagation_layer_1_delta: real;
begin
  forward_propagation_weight := (2 * Double(randint(1, 100))) - 1;
  forward_propagation_layer_1 := 0;
  forward_propagation_i := 0;
  while forward_propagation_i < forward_propagation_number_propagations do begin
  forward_propagation_layer_1 := sigmoid(INITIAL_VALUE * forward_propagation_weight);
  forward_propagation_layer_1_error := (Double(forward_propagation_expected) / 100) - forward_propagation_layer_1;
  forward_propagation_layer_1_delta := forward_propagation_layer_1_error * sigmoid_derivative(forward_propagation_layer_1);
  forward_propagation_weight := forward_propagation_weight + (INITIAL_VALUE * forward_propagation_layer_1_delta);
  forward_propagation_i := forward_propagation_i + 1;
end;
  exit(forward_propagation_layer_1 * 100);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  INITIAL_VALUE := 0.02;
  seed := 1;
  result_ := forward_propagation(32, 450000);
  writeln(result_);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
