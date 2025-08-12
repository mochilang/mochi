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
  density: real;
  diameter: real;
  viscosity: real;
  velocity: real;
  x: real;
function fabs(x: real): real; forward;
function reynolds_number(density: real; velocity: real; diameter: real; viscosity: real): real; forward;
function fabs(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end else begin
  exit(x);
end;
end;
function reynolds_number(density: real; velocity: real; diameter: real; viscosity: real): real;
begin
  if ((density <= 0) or (diameter <= 0)) or (viscosity <= 0) then begin
  panic('please ensure that density, diameter and viscosity are positive');
end;
  exit(((density * fabs(velocity)) * diameter) / viscosity);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(reynolds_number(900, 2.5, 0.05, 0.4));
  writeln(reynolds_number(450, 3.86, 0.078, 0.23));
  writeln(reynolds_number(234, -4.5, 0.3, 0.44));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
