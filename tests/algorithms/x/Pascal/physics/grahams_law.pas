{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  values: RealArray;
  r1: real;
  m1: real;
  rate: real;
  m2: real;
  mass: real;
  r2: real;
  x: real;
function to_float(x: integer): real; forward;
function round6(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function validate(values: RealArray): boolean; forward;
function effusion_ratio(m1: real; m2: real): real; forward;
function first_effusion_rate(rate: real; m1: real; m2: real): real; forward;
function second_effusion_rate(rate: real; m1: real; m2: real): real; forward;
function first_molar_mass(mass: real; r1: real; r2: real): real; forward;
function second_molar_mass(mass: real; r1: real; r2: real): real; forward;
function to_float(x: integer): real;
begin
  exit(x * 1);
end;
function round6(x: real): real;
var
  round6_factor: real;
begin
  round6_factor := 1e+06;
  exit(to_float(Trunc((x * round6_factor) + 0.5)) / round6_factor);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function validate(values: RealArray): boolean;
var
  validate_i: integer;
begin
  if Length(values) = 0 then begin
  exit(false);
end;
  validate_i := 0;
  while validate_i < Length(values) do begin
  if values[validate_i] <= 0 then begin
  exit(false);
end;
  validate_i := validate_i + 1;
end;
  exit(true);
end;
function effusion_ratio(m1: real; m2: real): real;
begin
  if not validate([m1, m2]) then begin
  writeln('ValueError: Molar mass values must greater than 0.');
  exit(0);
end;
  exit(round6(sqrtApprox(m2 / m1)));
end;
function first_effusion_rate(rate: real; m1: real; m2: real): real;
begin
  if not validate([rate, m1, m2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  exit(round6(rate * sqrtApprox(m2 / m1)));
end;
function second_effusion_rate(rate: real; m1: real; m2: real): real;
begin
  if not validate([rate, m1, m2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  exit(round6(rate / sqrtApprox(m2 / m1)));
end;
function first_molar_mass(mass: real; r1: real; r2: real): real;
var
  first_molar_mass_ratio: real;
begin
  if not validate([mass, r1, r2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  first_molar_mass_ratio := r1 / r2;
  exit(round6(mass / (first_molar_mass_ratio * first_molar_mass_ratio)));
end;
function second_molar_mass(mass: real; r1: real; r2: real): real;
var
  second_molar_mass_ratio: real;
begin
  if not validate([mass, r1, r2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  second_molar_mass_ratio := r1 / r2;
  exit(round6((second_molar_mass_ratio * second_molar_mass_ratio) / mass));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(effusion_ratio(2.016, 4.002));
  writeln(first_effusion_rate(1, 2.016, 4.002));
  writeln(second_effusion_rate(1, 2.016, 4.002));
  writeln(first_molar_mass(2, 1.408943, 0.709752));
  writeln(second_molar_mass(2, 1.408943, 0.709752));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
