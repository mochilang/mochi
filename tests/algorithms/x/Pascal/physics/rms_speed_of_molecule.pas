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
  UNIVERSAL_GAS_CONSTANT: real;
  vrms: real;
  x: real;
  temperature: real;
  molar_mass: real;
function sqrt(x: real): real; forward;
function rms_speed_of_molecule(temperature: real; molar_mass: real): real; forward;
function sqrt(x: real): real;
var
  sqrt_guess: real;
  sqrt_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_guess := x;
  sqrt_i := 0;
  while sqrt_i < 10 do begin
  sqrt_guess := (sqrt_guess + (x / sqrt_guess)) / 2;
  sqrt_i := sqrt_i + 1;
end;
  exit(sqrt_guess);
end;
function rms_speed_of_molecule(temperature: real; molar_mass: real): real;
var
  rms_speed_of_molecule_num: real;
  rms_speed_of_molecule_val: real;
  rms_speed_of_molecule_result_: real;
begin
  if temperature < 0 then begin
  panic('Temperature cannot be less than 0 K');
end;
  if molar_mass <= 0 then begin
  panic('Molar mass cannot be less than or equal to 0 kg/mol');
end;
  rms_speed_of_molecule_num := (3 * UNIVERSAL_GAS_CONSTANT) * temperature;
  rms_speed_of_molecule_val := rms_speed_of_molecule_num / molar_mass;
  rms_speed_of_molecule_result_ := sqrt(rms_speed_of_molecule_val);
  exit(rms_speed_of_molecule_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  UNIVERSAL_GAS_CONSTANT := 8.3144598;
  writeln('rms_speed_of_molecule(100, 2) = ' + FloatToStr(rms_speed_of_molecule(100, 2)));
  writeln('rms_speed_of_molecule(273, 12) = ' + FloatToStr(rms_speed_of_molecule(273, 12)));
  vrms := rms_speed_of_molecule(300, 28);
  writeln(('Vrms of Nitrogen gas at 300 K is ' + FloatToStr(vrms)) + ' m/s');
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
