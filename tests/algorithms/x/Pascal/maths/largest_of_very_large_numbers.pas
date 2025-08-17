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
  x: integer;
  x1: integer;
  x2: integer;
  y: integer;
  y1: integer;
  y2: integer;
function ln_(x: real): real; forward;
function log10(x: real): real; forward;
function absf(x: real): real; forward;
function res(x: integer; y: integer): real; forward;
procedure test_res(); forward;
function compare(x1: integer; y1: integer; x2: integer; y2: integer): string; forward;
function ln_(x: real): real;
var
  ln__t: real;
  ln__term: real;
  ln__sum: real;
  ln__k: integer;
begin
  ln__t := (x - 1) / (x + 1);
  ln__term := ln__t;
  ln__sum := 0;
  ln__k := 1;
  while ln__k <= 99 do begin
  ln__sum := ln__sum + (ln__term / Double(ln__k));
  ln__term := (ln__term * ln__t) * ln__t;
  ln__k := ln__k + 2;
end;
  exit(2 * ln__sum);
end;
function log10(x: real): real;
begin
  exit(ln(x) / ln(10));
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function res(x: integer; y: integer): real;
begin
  if x = 0 then begin
  exit(0);
end;
  if y = 0 then begin
  exit(1);
end;
  if x < 0 then begin
  panic('math domain error');
end;
  exit(Double(y) * log10(Double(x)));
end;
procedure test_res();
begin
  if absf(res(5, 7) - 4.892790030352132) > 1e-07 then begin
  panic('res(5,7) failed');
end;
  if res(0, 5) <> 0 then begin
  panic('res(0,5) failed');
end;
  if res(3, 0) <> 1 then begin
  panic('res(3,0) failed');
end;
end;
function compare(x1: integer; y1: integer; x2: integer; y2: integer): string;
var
  compare_r1: real;
  compare_r2: real;
begin
  compare_r1 := res(x1, y1);
  compare_r2 := res(x2, y2);
  if compare_r1 > compare_r2 then begin
  exit((('Largest number is ' + IntToStr(x1)) + ' ^ ') + IntToStr(y1));
end;
  if compare_r2 > compare_r1 then begin
  exit((('Largest number is ' + IntToStr(x2)) + ' ^ ') + IntToStr(y2));
end;
  exit('Both are equal');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  test_res();
  writeln(compare(5, 7, 4, 8));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
