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
function doppler_effect(doppler_effect_org_freq: real; doppler_effect_wave_vel: real; doppler_effect_obs_vel: real; doppler_effect_src_vel: real): real; forward;
function absf(absf_x: real): real; forward;
function almost_equal(almost_equal_a: real; almost_equal_b: real; almost_equal_tol: real): boolean; forward;
procedure test_doppler_effect(); forward;
procedure main(); forward;
function doppler_effect(doppler_effect_org_freq: real; doppler_effect_wave_vel: real; doppler_effect_obs_vel: real; doppler_effect_src_vel: real): real;
var
  doppler_effect_doppler_freq: real;
begin
  if doppler_effect_wave_vel = doppler_effect_src_vel then begin
  panic('division by zero implies vs=v and observer in front of the source');
end;
  doppler_effect_doppler_freq := (doppler_effect_org_freq * (doppler_effect_wave_vel + doppler_effect_obs_vel)) / (doppler_effect_wave_vel - doppler_effect_src_vel);
  if doppler_effect_doppler_freq <= 0 then begin
  panic('non-positive frequency implies vs>v or v0>v (in the opposite direction)');
end;
  exit(doppler_effect_doppler_freq);
end;
function absf(absf_x: real): real;
begin
  if absf_x < 0 then begin
  exit(-absf_x);
end;
  exit(absf_x);
end;
function almost_equal(almost_equal_a: real; almost_equal_b: real; almost_equal_tol: real): boolean;
begin
  exit(absf(almost_equal_a - almost_equal_b) <= almost_equal_tol);
end;
procedure test_doppler_effect();
begin
  if not almost_equal(doppler_effect(100, 330, 10, 0), 103.03030303030303, 1e-07) then begin
  panic('test 1 failed');
end;
  if not almost_equal(doppler_effect(100, 330, -10, 0), 96.96969696969697, 1e-07) then begin
  panic('test 2 failed');
end;
  if not almost_equal(doppler_effect(100, 330, 0, 10), 103.125, 1e-07) then begin
  panic('test 3 failed');
end;
  if not almost_equal(doppler_effect(100, 330, 0, -10), 97.05882352941177, 1e-07) then begin
  panic('test 4 failed');
end;
  if not almost_equal(doppler_effect(100, 330, 10, 10), 106.25, 1e-07) then begin
  panic('test 5 failed');
end;
  if not almost_equal(doppler_effect(100, 330, -10, -10), 94.11764705882354, 1e-07) then begin
  panic('test 6 failed');
end;
end;
procedure main();
begin
  test_doppler_effect();
  writeln(doppler_effect(100, 330, 10, 0));
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
