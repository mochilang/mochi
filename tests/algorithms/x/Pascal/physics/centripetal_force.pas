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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function centripetal(centripetal_mass: real; centripetal_velocity: real; centripetal_radius: real): real; forward;
function floor(floor_x: real): real; forward;
function pow10(pow10_n: int64): real; forward;
function round(round_x: real; round_n: int64): real; forward;
procedure show(show_mass: real; show_velocity: real; show_radius: real); forward;
function centripetal(centripetal_mass: real; centripetal_velocity: real; centripetal_radius: real): real;
begin
  if centripetal_mass < 0 then begin
  panic('The mass of the body cannot be negative');
end;
  if centripetal_radius <= 0 then begin
  panic('The radius is always a positive non zero integer');
end;
  exit(((centripetal_mass * centripetal_velocity) * centripetal_velocity) / centripetal_radius);
end;
function floor(floor_x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(floor_x);
  if Double(floor_i) > floor_x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(pow10_n: int64): real;
var
  pow10_p: real;
  pow10_i: int64;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round(round_x: real; round_n: int64): real;
var
  round_m: real;
begin
  round_m := pow10(round_n);
  exit(Floor((round_x * round_m) + 0.5) / round_m);
end;
procedure show(show_mass: real; show_velocity: real; show_radius: real);
var
  show_f: real;
begin
  show_f := centripetal(show_mass, show_velocity, show_radius);
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
  writeln('');
end.
