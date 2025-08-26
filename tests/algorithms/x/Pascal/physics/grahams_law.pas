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
function to_float(to_float_x: int64): real; forward;
function round6(round6_x: real): real; forward;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function validate(validate_values: RealArray): boolean; forward;
function effusion_ratio(effusion_ratio_m1: real; effusion_ratio_m2: real): real; forward;
function first_effusion_rate(first_effusion_rate_rate: real; first_effusion_rate_m1: real; first_effusion_rate_m2: real): real; forward;
function second_effusion_rate(second_effusion_rate_rate: real; second_effusion_rate_m1: real; second_effusion_rate_m2: real): real; forward;
function first_molar_mass(first_molar_mass_mass: real; first_molar_mass_r1: real; first_molar_mass_r2: real): real; forward;
function second_molar_mass(second_molar_mass_mass: real; second_molar_mass_r1: real; second_molar_mass_r2: real): real; forward;
function to_float(to_float_x: int64): real;
begin
  exit(to_float_x * 1);
end;
function round6(round6_x: real): real;
var
  round6_factor: real;
begin
  round6_factor := 1e+06;
  exit(to_float(Trunc((round6_x * round6_factor) + 0.5)) / round6_factor);
end;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: int64;
begin
  sqrtApprox_guess := sqrtApprox_x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function validate(validate_values: RealArray): boolean;
var
  validate_i: int64;
begin
  if Length(validate_values) = 0 then begin
  exit(false);
end;
  validate_i := 0;
  while validate_i < Length(validate_values) do begin
  if validate_values[validate_i] <= 0 then begin
  exit(false);
end;
  validate_i := validate_i + 1;
end;
  exit(true);
end;
function effusion_ratio(effusion_ratio_m1: real; effusion_ratio_m2: real): real;
begin
  if not validate([effusion_ratio_m1, effusion_ratio_m2]) then begin
  writeln('ValueError: Molar mass values must greater than 0.');
  exit(0);
end;
  exit(round6(sqrtApprox(effusion_ratio_m2 / effusion_ratio_m1)));
end;
function first_effusion_rate(first_effusion_rate_rate: real; first_effusion_rate_m1: real; first_effusion_rate_m2: real): real;
begin
  if not validate([first_effusion_rate_rate, first_effusion_rate_m1, first_effusion_rate_m2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  exit(round6(first_effusion_rate_rate * sqrtApprox(first_effusion_rate_m2 / first_effusion_rate_m1)));
end;
function second_effusion_rate(second_effusion_rate_rate: real; second_effusion_rate_m1: real; second_effusion_rate_m2: real): real;
begin
  if not validate([second_effusion_rate_rate, second_effusion_rate_m1, second_effusion_rate_m2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  exit(round6(second_effusion_rate_rate / sqrtApprox(second_effusion_rate_m2 / second_effusion_rate_m1)));
end;
function first_molar_mass(first_molar_mass_mass: real; first_molar_mass_r1: real; first_molar_mass_r2: real): real;
var
  first_molar_mass_ratio: real;
begin
  if not validate([first_molar_mass_mass, first_molar_mass_r1, first_molar_mass_r2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  first_molar_mass_ratio := first_molar_mass_r1 / first_molar_mass_r2;
  exit(round6(first_molar_mass_mass / (first_molar_mass_ratio * first_molar_mass_ratio)));
end;
function second_molar_mass(second_molar_mass_mass: real; second_molar_mass_r1: real; second_molar_mass_r2: real): real;
var
  second_molar_mass_ratio: real;
begin
  if not validate([second_molar_mass_mass, second_molar_mass_r1, second_molar_mass_r2]) then begin
  writeln('ValueError: Molar mass and effusion rate values must greater than 0.');
  exit(0);
end;
  second_molar_mass_ratio := second_molar_mass_r1 / second_molar_mass_r2;
  exit(round6((second_molar_mass_ratio * second_molar_mass_ratio) / second_molar_mass_mass));
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
  writeln('');
end.
