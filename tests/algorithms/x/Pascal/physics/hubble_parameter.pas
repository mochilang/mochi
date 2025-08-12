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
  matter_density: real;
  dark_energy: real;
  radiation_density: real;
  x: real;
  redshift: real;
  hubble_constant: real;
  exp: integer;
  base: real;
function pow(base: real; exp: integer): real; forward;
function sqrt_approx(x: real): real; forward;
function hubble_parameter(hubble_constant: real; radiation_density: real; matter_density: real; dark_energy: real; redshift: real): real; forward;
procedure test_hubble_parameter(); forward;
procedure main(); forward;
function pow(base: real; exp: integer): real;
var
  pow_result_: real;
  pow_i: integer;
begin
  pow_result_ := 1;
  pow_i := 0;
  while pow_i < exp do begin
  pow_result_ := pow_result_ * base;
  pow_i := pow_i + 1;
end;
  exit(pow_result_);
end;
function sqrt_approx(x: real): real;
var
  sqrt_approx_guess: real;
  sqrt_approx_i: integer;
begin
  if x = 0 then begin
  exit(0);
end;
  sqrt_approx_guess := x / 2;
  sqrt_approx_i := 0;
  while sqrt_approx_i < 20 do begin
  sqrt_approx_guess := (sqrt_approx_guess + (x / sqrt_approx_guess)) / 2;
  sqrt_approx_i := sqrt_approx_i + 1;
end;
  exit(sqrt_approx_guess);
end;
function hubble_parameter(hubble_constant: real; radiation_density: real; matter_density: real; dark_energy: real; redshift: real): real;
var
  hubble_parameter_parameters: array of real;
  hubble_parameter_i: integer;
  hubble_parameter_curvature: real;
  hubble_parameter_zp1: real;
  hubble_parameter_e2: real;
begin
  hubble_parameter_parameters := [redshift, radiation_density, matter_density, dark_energy];
  hubble_parameter_i := 0;
  while hubble_parameter_i < Length(hubble_parameter_parameters) do begin
  if hubble_parameter_parameters[hubble_parameter_i] < 0 then begin
  panic('All input parameters must be positive');
end;
  hubble_parameter_i := hubble_parameter_i + 1;
end;
  hubble_parameter_i := 1;
  while hubble_parameter_i < 4 do begin
  if hubble_parameter_parameters[hubble_parameter_i] > 1 then begin
  panic('Relative densities cannot be greater than one');
end;
  hubble_parameter_i := hubble_parameter_i + 1;
end;
  hubble_parameter_curvature := 1 - ((matter_density + radiation_density) + dark_energy);
  hubble_parameter_zp1 := redshift + 1;
  hubble_parameter_e2 := (((radiation_density * pow(hubble_parameter_zp1, 4)) + (matter_density * pow(hubble_parameter_zp1, 3))) + (hubble_parameter_curvature * pow(hubble_parameter_zp1, 2))) + dark_energy;
  exit(hubble_constant * sqrt_approx(hubble_parameter_e2));
end;
procedure test_hubble_parameter();
var
  test_hubble_parameter_h: real;
begin
  test_hubble_parameter_h := hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0);
  if (test_hubble_parameter_h < 68.2999) or (test_hubble_parameter_h > 68.3001) then begin
  panic('hubble_parameter test failed');
end;
end;
procedure main();
begin
  test_hubble_parameter();
  writeln(hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
