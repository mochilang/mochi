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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  mu: real;
  sigma: real;
  x: real;
function sqrtApprox(x: real): real; forward;
function expApprox(x: real): real; forward;
function gaussian(x: real; mu: real; sigma: real): real; forward;
procedure main(); forward;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function expApprox(x: real): real;
var
  expApprox_is_neg: boolean;
  expApprox_y: real;
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: integer;
begin
  expApprox_is_neg := false;
  expApprox_y := x;
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
function gaussian(x: real; mu: real; sigma: real): real;
var
  gaussian_coeff: real;
  gaussian_exponent: real;
begin
  gaussian_coeff := 1 / sqrtApprox(((2 * PI) * sigma) * sigma);
  gaussian_exponent := -((x - mu) * (x - mu)) / ((2 * sigma) * sigma);
  exit(gaussian_coeff * expApprox(gaussian_exponent));
end;
procedure main();
var
  main_result_: real;
begin
  main_result_ := gaussian(1, 0, 1);
  writeln(main_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
