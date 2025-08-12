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
  G: real;
  gravity: real;
  volume: real;
  fluid_density: real;
function archimedes_principle(fluid_density: real; volume: real; gravity: real): real; forward;
function archimedes_principle_default(fluid_density: real; volume: real): real; forward;
function archimedes_principle(fluid_density: real; volume: real; gravity: real): real;
begin
  if fluid_density <= 0 then begin
  panic('Impossible fluid density');
end;
  if volume <= 0 then begin
  panic('Impossible object volume');
end;
  if gravity < 0 then begin
  panic('Impossible gravity');
end;
  exit((fluid_density * volume) * gravity);
end;
function archimedes_principle_default(fluid_density: real; volume: real): real;
var
  archimedes_principle_default_res: real;
begin
  archimedes_principle_default_res := archimedes_principle(fluid_density, volume, G);
  exit(archimedes_principle_default_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  G := 9.80665;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
