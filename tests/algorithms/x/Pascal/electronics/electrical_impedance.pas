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
procedure json(x: int64);
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
function Map3(electrical_impedance_value_9: real): specialize TFPGMap<string, real>; forward;
function Map2(electrical_impedance_value_8: real): specialize TFPGMap<string, real>; forward;
function Map1(electrical_impedance_value: real): specialize TFPGMap<string, real>; forward;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function electrical_impedance(electrical_impedance_resistance: real; electrical_impedance_reactance: real; electrical_impedance_impedance: real): specialize TFPGMap<string, real>; forward;
function Map3(electrical_impedance_value_9: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('impedance', electrical_impedance_value_9);
end;
function Map2(electrical_impedance_value_8: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('reactance', electrical_impedance_value_8);
end;
function Map1(electrical_impedance_value: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('resistance', electrical_impedance_value);
end;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: int64;
begin
  if sqrtApprox_x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := sqrtApprox_x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function electrical_impedance(electrical_impedance_resistance: real; electrical_impedance_reactance: real; electrical_impedance_impedance: real): specialize TFPGMap<string, real>;
var
  electrical_impedance_zero_count: int64;
  electrical_impedance_value: real;
  electrical_impedance_value_8: real;
  electrical_impedance_value_9: real;
begin
  electrical_impedance_zero_count := 0;
  if electrical_impedance_resistance = 0 then begin
  electrical_impedance_zero_count := electrical_impedance_zero_count + 1;
end;
  if electrical_impedance_reactance = 0 then begin
  electrical_impedance_zero_count := electrical_impedance_zero_count + 1;
end;
  if electrical_impedance_impedance = 0 then begin
  electrical_impedance_zero_count := electrical_impedance_zero_count + 1;
end;
  if electrical_impedance_zero_count <> 1 then begin
  panic('One and only one argument must be 0');
end;
  if electrical_impedance_resistance = 0 then begin
  electrical_impedance_value := sqrtApprox((electrical_impedance_impedance * electrical_impedance_impedance) - (electrical_impedance_reactance * electrical_impedance_reactance));
  exit(Map1(electrical_impedance_value));
end else begin
  if electrical_impedance_reactance = 0 then begin
  electrical_impedance_value_8 := sqrtApprox((electrical_impedance_impedance * electrical_impedance_impedance) - (electrical_impedance_resistance * electrical_impedance_resistance));
  exit(Map2(electrical_impedance_value_8));
end else begin
  if electrical_impedance_impedance = 0 then begin
  electrical_impedance_value_9 := sqrtApprox((electrical_impedance_resistance * electrical_impedance_resistance) + (electrical_impedance_reactance * electrical_impedance_reactance));
  exit(Map3(electrical_impedance_value_9));
end else begin
  panic('Exactly one argument must be 0');
end;
end;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_map_real(electrical_impedance(3, 4, 0));
  show_map_real(electrical_impedance(0, 4, 5));
  show_map_real(electrical_impedance(3, 0, 5));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
