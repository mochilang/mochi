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
function pow(pow_base: real; pow_exp_: int64): real; forward;
function sqrt_approx(sqrt_approx_x: real): real; forward;
function hubble_parameter(hubble_parameter_hubble_constant: real; hubble_parameter_radiation_density: real; hubble_parameter_matter_density: real; hubble_parameter_dark_energy: real; hubble_parameter_redshift: real): real; forward;
procedure test_hubble_parameter(); forward;
procedure main(); forward;
function pow(pow_base: real; pow_exp_: int64): real;
var
  pow_result_: real;
  pow_i: int64;
begin
  pow_result_ := 1;
  pow_i := 0;
  while pow_i < pow_exp_ do begin
  pow_result_ := pow_result_ * pow_base;
  pow_i := pow_i + 1;
end;
  exit(pow_result_);
end;
function sqrt_approx(sqrt_approx_x: real): real;
var
  sqrt_approx_guess: real;
  sqrt_approx_i: int64;
begin
  if sqrt_approx_x = 0 then begin
  exit(0);
end;
  sqrt_approx_guess := sqrt_approx_x / 2;
  sqrt_approx_i := 0;
  while sqrt_approx_i < 20 do begin
  sqrt_approx_guess := (sqrt_approx_guess + (sqrt_approx_x / sqrt_approx_guess)) / 2;
  sqrt_approx_i := sqrt_approx_i + 1;
end;
  exit(sqrt_approx_guess);
end;
function hubble_parameter(hubble_parameter_hubble_constant: real; hubble_parameter_radiation_density: real; hubble_parameter_matter_density: real; hubble_parameter_dark_energy: real; hubble_parameter_redshift: real): real;
var
  hubble_parameter_parameters: array of real;
  hubble_parameter_i: int64;
  hubble_parameter_curvature: real;
  hubble_parameter_zp1: real;
  hubble_parameter_e2: real;
begin
  hubble_parameter_parameters := [hubble_parameter_redshift, hubble_parameter_radiation_density, hubble_parameter_matter_density, hubble_parameter_dark_energy];
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
  hubble_parameter_curvature := 1 - ((hubble_parameter_matter_density + hubble_parameter_radiation_density) + hubble_parameter_dark_energy);
  hubble_parameter_zp1 := hubble_parameter_redshift + 1;
  hubble_parameter_e2 := (((hubble_parameter_radiation_density * pow(hubble_parameter_zp1, 4)) + (hubble_parameter_matter_density * pow(hubble_parameter_zp1, 3))) + (hubble_parameter_curvature * pow(hubble_parameter_zp1, 2))) + hubble_parameter_dark_energy;
  exit(hubble_parameter_hubble_constant * sqrt_approx(hubble_parameter_e2));
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
  writeln('');
end.
