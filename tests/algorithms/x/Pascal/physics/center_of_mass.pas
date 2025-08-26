{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Particle = record
  x: real;
  y: real;
  z: real;
  mass: real;
end;
type Coord3D = record
  x: real;
  y: real;
  z: real;
end;
type ParticleArray = array of Particle;
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
  r1: Coord3D;
  r2: Coord3D;
function makeCoord3D(x: real; y: real; z: real): Coord3D; forward;
function makeParticle(x: real; y: real; z: real; mass: real): Particle; forward;
function round2(round2_x: real): real; forward;
function center_of_mass(center_of_mass_ps: ParticleArray): Coord3D; forward;
function coord_to_string(coord_to_string_c: Coord3D): string; forward;
function makeCoord3D(x: real; y: real; z: real): Coord3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;
function makeParticle(x: real; y: real; z: real; mass: real): Particle;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.mass := mass;
end;
function round2(round2_x: real): real;
var
  round2_scaled: real;
  round2_rounded: real;
begin
  round2_scaled := round2_x * 100;
  round2_rounded := Double(Trunc(round2_scaled + 0.5));
  exit(round2_rounded / 100);
end;
function center_of_mass(center_of_mass_ps: ParticleArray): Coord3D;
var
  center_of_mass_i: int64;
  center_of_mass_total_mass: real;
  center_of_mass_p: Particle;
  center_of_mass_sum_x: real;
  center_of_mass_sum_y: real;
  center_of_mass_sum_z: real;
  center_of_mass_cm_x: real;
  center_of_mass_cm_y: real;
  center_of_mass_cm_z: real;
begin
  if Length(center_of_mass_ps) = 0 then begin
  panic('No particles provided');
end;
  center_of_mass_i := 0;
  center_of_mass_total_mass := 0;
  while center_of_mass_i < Length(center_of_mass_ps) do begin
  center_of_mass_p := center_of_mass_ps[center_of_mass_i];
  if center_of_mass_p.mass <= 0 then begin
  panic('Mass of all particles must be greater than 0');
end;
  center_of_mass_total_mass := center_of_mass_total_mass + center_of_mass_p.mass;
  center_of_mass_i := center_of_mass_i + 1;
end;
  center_of_mass_sum_x := 0;
  center_of_mass_sum_y := 0;
  center_of_mass_sum_z := 0;
  center_of_mass_i := 0;
  while center_of_mass_i < Length(center_of_mass_ps) do begin
  center_of_mass_p := center_of_mass_ps[center_of_mass_i];
  center_of_mass_sum_x := center_of_mass_sum_x + (center_of_mass_p.x * center_of_mass_p.mass);
  center_of_mass_sum_y := center_of_mass_sum_y + (center_of_mass_p.y * center_of_mass_p.mass);
  center_of_mass_sum_z := center_of_mass_sum_z + (center_of_mass_p.z * center_of_mass_p.mass);
  center_of_mass_i := center_of_mass_i + 1;
end;
  center_of_mass_cm_x := round2(center_of_mass_sum_x / center_of_mass_total_mass);
  center_of_mass_cm_y := round2(center_of_mass_sum_y / center_of_mass_total_mass);
  center_of_mass_cm_z := round2(center_of_mass_sum_z / center_of_mass_total_mass);
  exit(makeCoord3D(center_of_mass_cm_x, center_of_mass_cm_y, center_of_mass_cm_z));
end;
function coord_to_string(coord_to_string_c: Coord3D): string;
begin
  exit(((((('Coord3D(x=' + FloatToStr(coord_to_string_c.x)) + ', y=') + FloatToStr(coord_to_string_c.y)) + ', z=') + FloatToStr(coord_to_string_c.z)) + ')');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := center_of_mass([makeParticle(1.5, 4, 3.4, 4), makeParticle(5, 6.8, 7, 8.1), makeParticle(9.4, 10.1, 11.6, 12)]);
  writeln(coord_to_string(r1));
  r2 := center_of_mass([makeParticle(1, 2, 3, 4), makeParticle(5, 6, 7, 8), makeParticle(9, 10, 11, 12)]);
  writeln(coord_to_string(r2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
