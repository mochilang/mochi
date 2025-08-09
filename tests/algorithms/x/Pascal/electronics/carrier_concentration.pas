{$mode objfpc}
program Main;
uses SysUtils;
type CarrierResult = record
  name: string;
  value: real;
end;
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
  r1: CarrierResult;
  r2: CarrierResult;
  r3: CarrierResult;
  intrinsic_conc: real;
  hole_conc: real;
  x: real;
  electron_conc: real;
function makeCarrierResult(name: string; value: real): CarrierResult; forward;
function sqrtApprox(x: real): real; forward;
function carrier_concentration(electron_conc: real; hole_conc: real; intrinsic_conc: real): CarrierResult; forward;
function makeCarrierResult(name: string; value: real): CarrierResult;
begin
  Result.name := name;
  Result.value := value;
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
function carrier_concentration(electron_conc: real; hole_conc: real; intrinsic_conc: real): CarrierResult;
var
  carrier_concentration_zero_count: integer;
begin
  carrier_concentration_zero_count := 0;
  if electron_conc = 0 then begin
  carrier_concentration_zero_count := carrier_concentration_zero_count + 1;
end;
  if hole_conc = 0 then begin
  carrier_concentration_zero_count := carrier_concentration_zero_count + 1;
end;
  if intrinsic_conc = 0 then begin
  carrier_concentration_zero_count := carrier_concentration_zero_count + 1;
end;
  if carrier_concentration_zero_count <> 1 then begin
  panic('You cannot supply more or less than 2 values');
end;
  if electron_conc < 0 then begin
  panic('Electron concentration cannot be negative in a semiconductor');
end;
  if hole_conc < 0 then begin
  panic('Hole concentration cannot be negative in a semiconductor');
end;
  if intrinsic_conc < 0 then begin
  panic('Intrinsic concentration cannot be negative in a semiconductor');
end;
  if electron_conc = 0 then begin
  exit(makeCarrierResult('electron_conc', (intrinsic_conc * intrinsic_conc) / hole_conc));
end;
  if hole_conc = 0 then begin
  exit(makeCarrierResult('hole_conc', (intrinsic_conc * intrinsic_conc) / electron_conc));
end;
  if intrinsic_conc = 0 then begin
  exit(makeCarrierResult('intrinsic_conc', sqrtApprox(electron_conc * hole_conc)));
end;
  exit(makeCarrierResult('', -1));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := carrier_concentration(25, 100, 0);
  writeln((r1.name + ', ') + FloatToStr(r1.value));
  r2 := carrier_concentration(0, 1600, 200);
  writeln((r2.name + ', ') + FloatToStr(r2.value));
  r3 := carrier_concentration(1000, 0, 1200);
  writeln((r3.name + ', ') + FloatToStr(r3.value));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
