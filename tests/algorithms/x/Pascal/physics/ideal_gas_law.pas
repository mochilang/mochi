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
  moles: real;
  kelvin: real;
  volume: real;
  pressure: real;
function pressure_of_gas_system(moles: real; kelvin: real; volume: real): real; forward;
function volume_of_gas_system(moles: real; kelvin: real; pressure: real): real; forward;
function temperature_of_gas_system(moles: real; volume: real; pressure: real): real; forward;
function moles_of_gas_system(kelvin: real; volume: real; pressure: real): real; forward;
function pressure_of_gas_system(moles: real; kelvin: real; volume: real): real;
begin
  if ((moles < 0) or (kelvin < 0)) or (volume < 0) then begin
  panic('Invalid inputs. Enter positive value.');
end;
  exit(((moles * kelvin) * UNIVERSAL_GAS_CONSTANT) / volume);
end;
function volume_of_gas_system(moles: real; kelvin: real; pressure: real): real;
begin
  if ((moles < 0) or (kelvin < 0)) or (pressure < 0) then begin
  panic('Invalid inputs. Enter positive value.');
end;
  exit(((moles * kelvin) * UNIVERSAL_GAS_CONSTANT) / pressure);
end;
function temperature_of_gas_system(moles: real; volume: real; pressure: real): real;
begin
  if ((moles < 0) or (volume < 0)) or (pressure < 0) then begin
  panic('Invalid inputs. Enter positive value.');
end;
  exit((pressure * volume) / (moles * UNIVERSAL_GAS_CONSTANT));
end;
function moles_of_gas_system(kelvin: real; volume: real; pressure: real): real;
begin
  if ((kelvin < 0) or (volume < 0)) or (pressure < 0) then begin
  panic('Invalid inputs. Enter positive value.');
end;
  exit((pressure * volume) / (kelvin * UNIVERSAL_GAS_CONSTANT));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  UNIVERSAL_GAS_CONSTANT := 8.314462;
  writeln(pressure_of_gas_system(2, 100, 5));
  writeln(volume_of_gas_system(0.5, 273, 0.004));
  writeln(temperature_of_gas_system(2, 100, 5));
  writeln(moles_of_gas_system(100, 5, 10));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
