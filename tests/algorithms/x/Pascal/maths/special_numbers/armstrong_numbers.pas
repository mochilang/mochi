{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
  base: integer;
  exp_: integer;
  n: integer;
function pow_int(base: integer; pow_int_exp_: integer): integer; forward;
function armstrong_number(n: integer): boolean; forward;
function pluperfect_number(n: integer): boolean; forward;
function narcissistic_number(n: integer): boolean; forward;
function pow_int(base: integer; pow_int_exp_: integer): integer;
var
  pow_int_result_: integer;
  pow_int_i: integer;
begin
  pow_int_result_ := 1;
  pow_int_i := 0;
  while pow_int_i < exp_ do begin
  pow_int_result_ := pow_int_result_ * base;
  pow_int_i := pow_int_i + 1;
end;
  exit(pow_int_result_);
end;
function armstrong_number(n: integer): boolean;
var
  armstrong_number_digits: integer;
  armstrong_number_temp: integer;
  armstrong_number_total: integer;
  armstrong_number_rem: integer;
begin
  if n < 1 then begin
  exit(false);
end;
  armstrong_number_digits := 0;
  armstrong_number_temp := n;
  while armstrong_number_temp > 0 do begin
  armstrong_number_temp := armstrong_number_temp div 10;
  armstrong_number_digits := armstrong_number_digits + 1;
end;
  armstrong_number_total := 0;
  armstrong_number_temp := n;
  while armstrong_number_temp > 0 do begin
  armstrong_number_rem := armstrong_number_temp mod 10;
  armstrong_number_total := armstrong_number_total + pow_int(armstrong_number_rem, armstrong_number_digits);
  armstrong_number_temp := armstrong_number_temp div 10;
end;
  exit(armstrong_number_total = n);
end;
function pluperfect_number(n: integer): boolean;
var
  pluperfect_number_digit_histogram: array of integer;
  pluperfect_number_i: integer;
  pluperfect_number_digit_total: integer;
  pluperfect_number_temp: integer;
  pluperfect_number_rem: integer;
  pluperfect_number_total: integer;
begin
  if n < 1 then begin
  exit(false);
end;
  pluperfect_number_digit_histogram := [];
  pluperfect_number_i := 0;
  while pluperfect_number_i < 10 do begin
  pluperfect_number_digit_histogram := concat(pluperfect_number_digit_histogram, IntArray([0]));
  pluperfect_number_i := pluperfect_number_i + 1;
end;
  pluperfect_number_digit_total := 0;
  pluperfect_number_temp := n;
  while pluperfect_number_temp > 0 do begin
  pluperfect_number_rem := pluperfect_number_temp mod 10;
  pluperfect_number_digit_histogram[pluperfect_number_rem] := pluperfect_number_digit_histogram[pluperfect_number_rem] + 1;
  pluperfect_number_digit_total := pluperfect_number_digit_total + 1;
  pluperfect_number_temp := pluperfect_number_temp div 10;
end;
  pluperfect_number_total := 0;
  pluperfect_number_i := 0;
  while pluperfect_number_i < 10 do begin
  if pluperfect_number_digit_histogram[pluperfect_number_i] > 0 then begin
  pluperfect_number_total := pluperfect_number_total + (pluperfect_number_digit_histogram[pluperfect_number_i] * pow_int(pluperfect_number_i, pluperfect_number_digit_total));
end;
  pluperfect_number_i := pluperfect_number_i + 1;
end;
  exit(pluperfect_number_total = n);
end;
function narcissistic_number(n: integer): boolean;
var
  narcissistic_number_digits: integer;
  narcissistic_number_temp: integer;
  narcissistic_number_total: integer;
  narcissistic_number_rem: integer;
begin
  if n < 1 then begin
  exit(false);
end;
  narcissistic_number_digits := 0;
  narcissistic_number_temp := n;
  while narcissistic_number_temp > 0 do begin
  narcissistic_number_temp := narcissistic_number_temp div 10;
  narcissistic_number_digits := narcissistic_number_digits + 1;
end;
  narcissistic_number_temp := n;
  narcissistic_number_total := 0;
  while narcissistic_number_temp > 0 do begin
  narcissistic_number_rem := narcissistic_number_temp mod 10;
  narcissistic_number_total := narcissistic_number_total + pow_int(narcissistic_number_rem, narcissistic_number_digits);
  narcissistic_number_temp := narcissistic_number_temp div 10;
end;
  exit(narcissistic_number_total = n);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(Ord(armstrong_number(371)));
  writeln(Ord(armstrong_number(200)));
  writeln(Ord(pluperfect_number(371)));
  writeln(Ord(pluperfect_number(200)));
  writeln(Ord(narcissistic_number(371)));
  writeln(Ord(narcissistic_number(200)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
