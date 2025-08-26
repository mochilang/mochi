{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
procedure show_map_real(m: specialize TFPGMap<string, real>);
var i: integer;
begin
  write('map[');
  for i := 0 to m.Count - 1 do begin
    write(m.Keys[i]);
    write(':');
    write(m.Data[i]);
    if i < m.Count - 1 then write(' ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  REDUCED_PLANCK_CONSTANT: real;
  SPEED_OF_LIGHT: real;
function Map3(casimir_force_d: real): specialize TFPGMap<string, real>; forward;
function Map2(casimir_force_a: real): specialize TFPGMap<string, real>; forward;
function Map1(casimir_force_f: real): specialize TFPGMap<string, real>; forward;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function casimir_force(casimir_force_force: real; casimir_force_area: real; casimir_force_distance: real): specialize TFPGMap<string, real>; forward;
procedure main(); forward;
function Map3(casimir_force_d: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('distance', casimir_force_d);
end;
function Map2(casimir_force_a: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('area', casimir_force_a);
end;
function Map1(casimir_force_f: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('force', casimir_force_f);
end;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: int64;
begin
  if sqrtApprox_x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := sqrtApprox_x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 100 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function casimir_force(casimir_force_force: real; casimir_force_area: real; casimir_force_distance: real): specialize TFPGMap<string, real>;
var
  casimir_force_zero_count: int64;
  casimir_force_num: real;
  casimir_force_den: real;
  casimir_force_f: real;
  casimir_force_num_13: real;
  casimir_force_den_14: real;
  casimir_force_a: real;
  casimir_force_num_16: real;
  casimir_force_den_17: real;
  casimir_force_inner: real;
  casimir_force_d: real;
begin
  casimir_force_zero_count := 0;
  if casimir_force_force = 0 then begin
  casimir_force_zero_count := casimir_force_zero_count + 1;
end;
  if casimir_force_area = 0 then begin
  casimir_force_zero_count := casimir_force_zero_count + 1;
end;
  if casimir_force_distance = 0 then begin
  casimir_force_zero_count := casimir_force_zero_count + 1;
end;
  if casimir_force_zero_count <> 1 then begin
  panic('One and only one argument must be 0');
end;
  if casimir_force_force < 0 then begin
  panic('Magnitude of force can not be negative');
end;
  if casimir_force_distance < 0 then begin
  panic('Distance can not be negative');
end;
  if casimir_force_area < 0 then begin
  panic('Area can not be negative');
end;
  if casimir_force_force = 0 then begin
  casimir_force_num := (((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI) * casimir_force_area;
  casimir_force_den := (((240 * casimir_force_distance) * casimir_force_distance) * casimir_force_distance) * casimir_force_distance;
  casimir_force_f := casimir_force_num / casimir_force_den;
  exit(Map1(casimir_force_f));
end;
  if casimir_force_area = 0 then begin
  casimir_force_num_13 := ((((240 * casimir_force_force) * casimir_force_distance) * casimir_force_distance) * casimir_force_distance) * casimir_force_distance;
  casimir_force_den_14 := ((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI;
  casimir_force_a := casimir_force_num_13 / casimir_force_den_14;
  exit(Map2(casimir_force_a));
end;
  casimir_force_num_16 := (((REDUCED_PLANCK_CONSTANT * SPEED_OF_LIGHT) * PI) * PI) * casimir_force_area;
  casimir_force_den_17 := 240 * casimir_force_force;
  casimir_force_inner := casimir_force_num_16 / casimir_force_den_17;
  casimir_force_d := sqrtApprox(sqrtApprox(casimir_force_inner));
  exit(Map3(casimir_force_d));
end;
procedure main();
begin
  show_map_real(casimir_force(0, 4, 0.03));
  show_map_real(casimir_force(2.635e-10, 0.0023, 0));
  show_map_real(casimir_force(2.737e-18, 0, 0.0023746));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  REDUCED_PLANCK_CONSTANT := 1.054571817e-34;
  SPEED_OF_LIGHT := 3e+08;
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
