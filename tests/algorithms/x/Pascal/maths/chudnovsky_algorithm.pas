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
  n: integer;
  x: real;
function sqrtApprox(x: real): real; forward;
function factorial_float(n: integer): real; forward;
function pi(n: integer): real; forward;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function factorial_float(n: integer): real;
var
  factorial_float_result_: real;
  factorial_float_i: integer;
begin
  factorial_float_result_ := 1;
  factorial_float_i := 2;
  while factorial_float_i <= n do begin
  factorial_float_result_ := factorial_float_result_ * Double(factorial_float_i);
  factorial_float_i := factorial_float_i + 1;
end;
  exit(factorial_float_result_);
end;
function pi(n: integer): real;
var
  pi_iterations: integer;
  pi_constant_term: real;
  pi_exponential_term: real;
  pi_linear_term: real;
  pi_partial_sum: real;
  pi_k: integer;
  pi_k6: integer;
  pi_k3: integer;
  pi_fact6k: real;
  pi_fact3k: real;
  pi_factk: real;
  pi_multinomial: real;
begin
  if n < 1 then begin
  panic('Undefined for non-natural numbers');
end;
  pi_iterations := (n + 13) div 14;
  pi_constant_term := 426880 * sqrtApprox(10005);
  pi_exponential_term := 1;
  pi_linear_term := 1.3591409e+07;
  pi_partial_sum := pi_linear_term;
  pi_k := 1;
  while pi_k < pi_iterations do begin
  pi_k6 := 6 * pi_k;
  pi_k3 := 3 * pi_k;
  pi_fact6k := factorial_float(pi_k6);
  pi_fact3k := factorial_float(pi_k3);
  pi_factk := factorial_float(pi_k);
  pi_multinomial := pi_fact6k / (((pi_fact3k * pi_factk) * pi_factk) * pi_factk);
  pi_linear_term := pi_linear_term + 5.45140134e+08;
  pi_exponential_term := pi_exponential_term * -2.62537412640768e+17;
  pi_partial_sum := pi_partial_sum + ((pi_multinomial * pi_linear_term) / pi_exponential_term);
  pi_k := pi_k + 1;
end;
  exit(pi_constant_term / pi_partial_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  n := 50;
  writeln((('The first ' + IntToStr(n)) + ' digits of pi is: ') + FloatToStr(pi(n)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
