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
  prob: real;
  trials: integer;
  n: integer;
  exp_: integer;
  x: real;
  successes: integer;
  base: real;
function abs(x: real): real; forward;
function factorial(n: integer): integer; forward;
function pow_float(base: real; pow_float_exp_: integer): real; forward;
function binomial_distribution(successes: integer; trials: integer; prob: real): real; forward;
function abs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function factorial(n: integer): integer;
var
  factorial_result_: integer;
  factorial_i: integer;
begin
  if n < 0 then begin
  panic('factorial is undefined for negative numbers');
end;
  factorial_result_ := 1;
  factorial_i := 2;
  while factorial_i <= n do begin
  factorial_result_ := factorial_result_ * factorial_i;
  factorial_i := factorial_i + 1;
end;
  exit(factorial_result_);
end;
function pow_float(base: real; pow_float_exp_: integer): real;
var
  pow_float_result_: real;
  pow_float_i: integer;
begin
  pow_float_result_ := 1;
  pow_float_i := 0;
  while pow_float_i < exp_ do begin
  pow_float_result_ := pow_float_result_ * base;
  pow_float_i := pow_float_i + 1;
end;
  exit(pow_float_result_);
end;
function binomial_distribution(successes: integer; trials: integer; prob: real): real;
var
  binomial_distribution_probability: real;
  binomial_distribution_numerator: real;
  binomial_distribution_denominator: real;
  binomial_distribution_coefficient: real;
begin
  if successes > trials then begin
  panic('successes must be lower or equal to trials');
end;
  if (trials < 0) or (successes < 0) then begin
  panic('the function is defined for non-negative integers');
end;
  if not ((0 < prob) and (prob < 1)) then begin
  panic('prob has to be in range of 1 - 0');
end;
  binomial_distribution_probability := pow_float(prob, successes) * pow_float(1 - prob, trials - successes);
  binomial_distribution_numerator := Double(factorial(trials));
  binomial_distribution_denominator := Double(factorial(successes) * factorial(trials - successes));
  binomial_distribution_coefficient := binomial_distribution_numerator / binomial_distribution_denominator;
  exit(binomial_distribution_probability * binomial_distribution_coefficient);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
