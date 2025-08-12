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
  mass: real;
  radius: real;
  x: real;
  n: integer;
  velocity: real;
function centripetal(mass: real; velocity: real; radius: real): real; forward;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round(x: real; n: integer): real; forward;
procedure show(mass: real; velocity: real; radius: real); forward;
function centripetal(mass: real; velocity: real; radius: real): real;
begin
  if mass < 0 then begin
  panic('The mass of the body cannot be negative');
end;
  if radius <= 0 then begin
  panic('The radius is always a positive non zero integer');
end;
  exit(((mass * velocity) * velocity) / radius);
end;
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
function pow10(n: integer): real;
var
  pow10_p: real;
  pow10_i: integer;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round(x: real; n: integer): real;
var
  round_m: real;
begin
  round_m := pow10(n);
  exit(Floor((x * round_m) + 0.5) / round_m);
end;
procedure show(mass: real; velocity: real; radius: real);
var
  show_f: real;
begin
  show_f := centripetal(mass, velocity, radius);
  writeln(FloatToStr(round(show_f, 2)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show(15.5, -30, 10);
  show(10, 15, 5);
  show(20, -50, 15);
  show(12.25, 40, 25);
  show(50, 100, 50);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
