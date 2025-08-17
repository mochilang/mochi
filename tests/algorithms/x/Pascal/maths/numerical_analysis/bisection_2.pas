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
  a: real;
  b: real;
  x: real;
function equation(x: real): real; forward;
function bisection(a: real; b: real): real; forward;
function equation(x: real): real;
begin
  exit(10 - (x * x));
end;
function bisection(a: real; b: real): real;
var
  bisection_left: real;
  bisection_right: real;
  bisection_c: real;
begin
  if (equation(a) * equation(b)) >= 0 then begin
  panic('Wrong space!');
end;
  bisection_left := a;
  bisection_right := b;
  bisection_c := bisection_left;
  while (bisection_right - bisection_left) >= 0.01 do begin
  bisection_c := (bisection_left + bisection_right) / 2;
  if equation(bisection_c) = 0 then begin
  break;
end;
  if (equation(bisection_c) * equation(bisection_left)) < 0 then begin
  bisection_right := bisection_c;
end else begin
  bisection_left := bisection_c;
end;
end;
  exit(bisection_c);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(bisection(-2, 5));
  writeln(bisection(0, 6));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
