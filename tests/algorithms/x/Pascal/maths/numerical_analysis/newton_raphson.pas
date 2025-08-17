{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type NRResult = record
  root: real;
  error: real;
  steps: array of real;
end;
type FuncType1 = function(arg0: real): real is nested;
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
  result_: NRResult;
  delta_x: real;
  f: FuncType1;
  log_steps: boolean;
  max_error: real;
  max_iter: integer;
  msg: string;
  step: real;
  x: real;
  x0: real;
function makeNRResult(root: real; error: real; steps: RealArray): NRResult; forward;
function abs_float(x: real): real; forward;
procedure fail(msg: string); forward;
function calc_derivative(f: FuncType1; x: real; delta_x: real): real; forward;
function newton_raphson(f: FuncType1; x0: real; max_iter: integer; step: real; max_error: real; log_steps: boolean): NRResult; forward;
function poly(x: real): real; forward;
function makeNRResult(root: real; error: real; steps: RealArray): NRResult;
begin
  Result.root := root;
  Result.error := error;
  Result.steps := steps;
end;
function abs_float(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
procedure fail(msg: string);
begin
  writeln('error: ' + msg);
end;
function calc_derivative(f: FuncType1; x: real; delta_x: real): real;
begin
  exit((f(x + (delta_x / 2)) - f(x - (delta_x / 2))) / delta_x);
end;
function newton_raphson(f: FuncType1; x0: real; max_iter: integer; step: real; max_error: real; log_steps: boolean): NRResult;
var
  newton_raphson_a: real;
  newton_raphson_steps: array of real;
  newton_raphson_i: integer;
  newton_raphson_err: real;
  newton_raphson_der: real;
begin
  newton_raphson_a := x0;
  newton_raphson_steps := [];
  newton_raphson_i := 0;
  while newton_raphson_i < max_iter do begin
  if log_steps then begin
  newton_raphson_steps := concat(newton_raphson_steps, [newton_raphson_a]);
end;
  newton_raphson_err := abs_float(f(newton_raphson_a));
  if newton_raphson_err < max_error then begin
  exit(makeNRResult(newton_raphson_a, newton_raphson_err, newton_raphson_steps));
end;
  newton_raphson_der := calc_derivative(f, newton_raphson_a, step);
  if newton_raphson_der = 0 then begin
  fail('No converging solution found, zero derivative');
  exit(makeNRResult(newton_raphson_a, newton_raphson_err, newton_raphson_steps));
end;
  newton_raphson_a := newton_raphson_a - (f(newton_raphson_a) / newton_raphson_der);
  newton_raphson_i := newton_raphson_i + 1;
end;
  fail('No converging solution found, iteration limit reached');
  exit(makeNRResult(newton_raphson_a, abs_float(f(newton_raphson_a)), newton_raphson_steps));
end;
function poly(x: real): real;
begin
  exit(((x * x) - (5 * x)) + 2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  result_ := newton_raphson(@poly, 0.4, 20, 1e-06, 1e-06, false);
  writeln((('root = ' + FloatToStr(result_.root)) + ', error = ') + FloatToStr(result_.error));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
