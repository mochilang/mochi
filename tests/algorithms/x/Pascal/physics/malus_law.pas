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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
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
  PI: real;
  TWO_PI: real;
function _mod(_mod_x: real; _mod_m: real): real; forward;
function cos(cos_x: real): real; forward;
function radians(radians_deg: real): real; forward;
function abs_val(abs_val_x: real): real; forward;
function malus_law(malus_law_initial_intensity: real; malus_law_angle: real): real; forward;
procedure main(); forward;
function _mod(_mod_x: real; _mod_m: real): real;
begin
  exit(_mod_x - (Floor(_mod_x / _mod_m) * _mod_m));
end;
function cos(cos_x: real): real;
var
  cos_y: real;
  cos_y2: real;
  cos_y4: real;
  cos_y6: real;
begin
  cos_y := _mod(cos_x + PI, TWO_PI) - PI;
  cos_y2 := cos_y * cos_y;
  cos_y4 := cos_y2 * cos_y2;
  cos_y6 := cos_y4 * cos_y2;
  exit(((1 - (cos_y2 / 2)) + (cos_y4 / 24)) - (cos_y6 / 720));
end;
function radians(radians_deg: real): real;
begin
  exit((radians_deg * PI) / 180);
end;
function abs_val(abs_val_x: real): real;
begin
  if abs_val_x < 0 then begin
  exit(-abs_val_x);
end;
  exit(abs_val_x);
end;
function malus_law(malus_law_initial_intensity: real; malus_law_angle: real): real;
var
  malus_law_theta: real;
  malus_law_c: real;
begin
  if malus_law_initial_intensity < 0 then begin
  panic('The value of intensity cannot be negative');
end;
  if (malus_law_angle < 0) or (malus_law_angle > 360) then begin
  panic('In Malus Law, the angle is in the range 0-360 degrees');
end;
  malus_law_theta := radians(malus_law_angle);
  malus_law_c := cos(malus_law_theta);
  exit(malus_law_initial_intensity * (malus_law_c * malus_law_c));
end;
procedure main();
begin
  writeln(FloatToStr(malus_law(100, 60)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  TWO_PI := 6.283185307179586;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
