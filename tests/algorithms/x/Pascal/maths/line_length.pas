{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type FuncType1 = function(arg0: real): real is nested;
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
  fnc: FuncType1;
  b: real;
  x_end: real;
  x_start: real;
  steps: integer;
  x: real;
  a: real;
  n: real;
function sqrt_newton(n: real): real; forward;
function hypot(a: real; b: real): real; forward;
function line_length(fnc: FuncType1; x_start: real; x_end: real; steps: integer): real; forward;
function f1(x: real): real; forward;
function f2(x: real): real; forward;
function f3(x: real): real; forward;
function sqrt_newton(n: real): real;
var
  sqrt_newton_x: real;
  sqrt_newton_i: integer;
begin
  if n = 0 then begin
  exit(0);
end;
  sqrt_newton_x := n;
  sqrt_newton_i := 0;
  while sqrt_newton_i < 20 do begin
  sqrt_newton_x := (sqrt_newton_x + (n / sqrt_newton_x)) / 2;
  sqrt_newton_i := sqrt_newton_i + 1;
end;
  exit(sqrt_newton_x);
end;
function hypot(a: real; b: real): real;
begin
  exit(sqrt_newton((a * a) + (b * b)));
end;
function line_length(fnc: FuncType1; x_start: real; x_end: real; steps: integer): real;
var
  line_length_x1: real;
  line_length_fx1: real;
  line_length_length_: real;
  line_length_i: integer;
  line_length_step: real;
  line_length_x2: real;
  line_length_fx2: real;
begin
  line_length_x1 := x_start;
  line_length_fx1 := fnc(x_start);
  line_length_length_ := 0;
  line_length_i := 0;
  line_length_step := (x_end - x_start) / (1 * steps);
  while line_length_i < steps do begin
  line_length_x2 := line_length_step + line_length_x1;
  line_length_fx2 := fnc(line_length_x2);
  line_length_length_ := line_length_length_ + hypot(line_length_x2 - line_length_x1, line_length_fx2 - line_length_fx1);
  line_length_x1 := line_length_x2;
  line_length_fx1 := line_length_fx2;
  line_length_i := line_length_i + 1;
end;
  exit(line_length_length_);
end;
function f1(x: real): real;
begin
  exit(x);
end;
function f2(x: real): real;
begin
  exit(1);
end;
function f3(x: real): real;
begin
  exit((x * x) / 10);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(line_length(@f1, 0, 1, 10));
  writeln(line_length(@f2, -5.5, 4.5, 100));
  writeln(line_length(@f3, 0, 10, 1000));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
