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
  PI: real;
function Map3(PI: real; ind_reactance_frequency: real; ind_reactance_inductance: real): specialize TFPGMap<string, real>; forward;
function Map2(PI: real; ind_reactance_inductance: real; ind_reactance_reactance: real): specialize TFPGMap<string, real>; forward;
function Map1(PI: real; ind_reactance_frequency: real; ind_reactance_reactance: real): specialize TFPGMap<string, real>; forward;
function ind_reactance(ind_reactance_inductance: real; ind_reactance_frequency: real; ind_reactance_reactance: real): specialize TFPGMap<string, real>; forward;
function Map3(PI: real; ind_reactance_frequency: real; ind_reactance_inductance: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('reactance', ((2 * PI) * ind_reactance_frequency) * ind_reactance_inductance);
end;
function Map2(PI: real; ind_reactance_inductance: real; ind_reactance_reactance: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('frequency', ind_reactance_reactance / ((2 * PI) * ind_reactance_inductance));
end;
function Map1(PI: real; ind_reactance_frequency: real; ind_reactance_reactance: real): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('inductance', ind_reactance_reactance / ((2 * PI) * ind_reactance_frequency));
end;
function ind_reactance(ind_reactance_inductance: real; ind_reactance_frequency: real; ind_reactance_reactance: real): specialize TFPGMap<string, real>;
var
  ind_reactance_zero_count: int64;
begin
  ind_reactance_zero_count := 0;
  if ind_reactance_inductance = 0 then begin
  ind_reactance_zero_count := ind_reactance_zero_count + 1;
end;
  if ind_reactance_frequency = 0 then begin
  ind_reactance_zero_count := ind_reactance_zero_count + 1;
end;
  if ind_reactance_reactance = 0 then begin
  ind_reactance_zero_count := ind_reactance_zero_count + 1;
end;
  if ind_reactance_zero_count <> 1 then begin
  panic('One and only one argument must be 0');
end;
  if ind_reactance_inductance < 0 then begin
  panic('Inductance cannot be negative');
end;
  if ind_reactance_frequency < 0 then begin
  panic('Frequency cannot be negative');
end;
  if ind_reactance_reactance < 0 then begin
  panic('Inductive reactance cannot be negative');
end;
  if ind_reactance_inductance = 0 then begin
  exit(Map1(PI, ind_reactance_frequency, ind_reactance_reactance));
end;
  if ind_reactance_frequency = 0 then begin
  exit(Map2(PI, ind_reactance_inductance, ind_reactance_reactance));
end;
  exit(Map3(PI, ind_reactance_frequency, ind_reactance_inductance));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  show_map_real(ind_reactance(0, 10000, 50));
  show_map_real(ind_reactance(0.035, 0, 50));
  show_map_real(ind_reactance(3.5e-05, 1000, 0));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
