{$mode objfpc}
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
  nfactor: real;
  temperature: real;
  pressure: real;
  x: real;
  volume: real;
  moles: real;
function round_to_int(x: real): integer; forward;
function molarity_to_normality(nfactor: real; moles: real; volume: real): integer; forward;
function moles_to_pressure(volume: real; moles: real; temperature: real): integer; forward;
function moles_to_volume(pressure: real; moles: real; temperature: real): integer; forward;
function pressure_and_volume_to_temperature(pressure: real; moles: real; volume: real): integer; forward;
function round_to_int(x: real): integer;
begin
  if x >= 0 then begin
  exit(Trunc(x + 0.5));
end;
  exit(Trunc(x - 0.5));
end;
function molarity_to_normality(nfactor: real; moles: real; volume: real): integer;
begin
  exit(round_to_int((moles / volume) * nfactor));
end;
function moles_to_pressure(volume: real; moles: real; temperature: real): integer;
begin
  exit(round_to_int(((moles * 0.0821) * temperature) / volume));
end;
function moles_to_volume(pressure: real; moles: real; temperature: real): integer;
begin
  exit(round_to_int(((moles * 0.0821) * temperature) / pressure));
end;
function pressure_and_volume_to_temperature(pressure: real; moles: real; volume: real): integer;
begin
  exit(round_to_int((pressure * volume) / (0.0821 * moles)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(molarity_to_normality(2, 3.1, 0.31)));
  writeln(IntToStr(molarity_to_normality(4, 11.4, 5.7)));
  writeln(IntToStr(moles_to_pressure(0.82, 3, 300)));
  writeln(IntToStr(moles_to_pressure(8.2, 5, 200)));
  writeln(IntToStr(moles_to_volume(0.82, 3, 300)));
  writeln(IntToStr(moles_to_volume(8.2, 5, 200)));
  writeln(IntToStr(pressure_and_volume_to_temperature(0.82, 1, 2)));
  writeln(IntToStr(pressure_and_volume_to_temperature(8.2, 5, 3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
