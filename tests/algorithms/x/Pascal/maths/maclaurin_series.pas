{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
  n: integer;
  theta: real;
  accuracy: integer;
  x: real;
function floor(x: real): real; forward;
function pow(x: real; n: integer): real; forward;
function factorial(n: integer): real; forward;
function maclaurin_sin(theta: real; accuracy: integer): real; forward;
function maclaurin_cos(theta: real; accuracy: integer): real; forward;
function floor(x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if Double(floor_i) > x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow(x: real; n: integer): real;
var
  pow_result_: real;
  pow_i: integer;
begin
  pow_result_ := 1;
  pow_i := 0;
  while pow_i < n do begin
  pow_result_ := pow_result_ * x;
  pow_i := pow_i + 1;
end;
  exit(pow_result_);
end;
function factorial(n: integer): real;
var
  factorial_result_: real;
  factorial_i: integer;
begin
  factorial_result_ := 1;
  factorial_i := 2;
  while factorial_i <= n do begin
  factorial_result_ := factorial_result_ * Double(factorial_i);
  factorial_i := factorial_i + 1;
end;
  exit(factorial_result_);
end;
function maclaurin_sin(theta: real; accuracy: integer): real;
var
  maclaurin_sin_t: real;
  maclaurin_sin_div_: real;
  maclaurin_sin_sum: real;
  maclaurin_sin_r: integer;
  maclaurin_sin_power: integer;
  maclaurin_sin_sign: real;
begin
  maclaurin_sin_t := theta;
  maclaurin_sin_div_ := Floor(maclaurin_sin_t / (2 * PI));
  maclaurin_sin_t := maclaurin_sin_t - ((2 * maclaurin_sin_div_) * PI);
  maclaurin_sin_sum := 0;
  maclaurin_sin_r := 0;
  while maclaurin_sin_r < accuracy do begin
  maclaurin_sin_power := (2 * maclaurin_sin_r) + 1;
  if (maclaurin_sin_r mod 2) = 0 then begin
  maclaurin_sin_sign := 1;
end else begin
  maclaurin_sin_sign := -1;
end;
  maclaurin_sin_sum := maclaurin_sin_sum + ((maclaurin_sin_sign * pow(maclaurin_sin_t, maclaurin_sin_power)) / factorial(maclaurin_sin_power));
  maclaurin_sin_r := maclaurin_sin_r + 1;
end;
  exit(maclaurin_sin_sum);
end;
function maclaurin_cos(theta: real; accuracy: integer): real;
var
  maclaurin_cos_t: real;
  maclaurin_cos_div_: real;
  maclaurin_cos_sum: real;
  maclaurin_cos_r: integer;
  maclaurin_cos_power: integer;
  maclaurin_cos_sign: real;
begin
  maclaurin_cos_t := theta;
  maclaurin_cos_div_ := Floor(maclaurin_cos_t / (2 * PI));
  maclaurin_cos_t := maclaurin_cos_t - ((2 * maclaurin_cos_div_) * PI);
  maclaurin_cos_sum := 0;
  maclaurin_cos_r := 0;
  while maclaurin_cos_r < accuracy do begin
  maclaurin_cos_power := 2 * maclaurin_cos_r;
  if (maclaurin_cos_r mod 2) = 0 then begin
  maclaurin_cos_sign := 1;
end else begin
  maclaurin_cos_sign := -1;
end;
  maclaurin_cos_sum := maclaurin_cos_sum + ((maclaurin_cos_sign * pow(maclaurin_cos_t, maclaurin_cos_power)) / factorial(maclaurin_cos_power));
  maclaurin_cos_r := maclaurin_cos_r + 1;
end;
  exit(maclaurin_cos_sum);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  writeln(FloatToStr(maclaurin_sin(10, 30)));
  writeln(FloatToStr(maclaurin_sin(-10, 30)));
  writeln(FloatToStr(maclaurin_sin(10, 15)));
  writeln(FloatToStr(maclaurin_sin(-10, 15)));
  writeln(FloatToStr(maclaurin_cos(5, 30)));
  writeln(FloatToStr(maclaurin_cos(-5, 30)));
  writeln(FloatToStr(maclaurin_cos(10, 15)));
  writeln(FloatToStr(maclaurin_cos(-10, 15)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
