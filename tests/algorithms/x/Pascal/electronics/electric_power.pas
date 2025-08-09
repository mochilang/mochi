{$mode objfpc}
program Main;
uses SysUtils, Math;
type Result = record
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
  r1: Result;
  r2: Result;
  r3: Result;
  r4: Result;
  r5: Result;
  power: real;
  x: real;
  current: real;
  n: integer;
  r: Result;
  voltage: real;
function makeResult(name: string; value: real): Result; forward;
function absf(x: real): real; forward;
function pow10(n: integer): real; forward;
function round_to(x: real; n: integer): real; forward;
function electric_power(voltage: real; current: real; power: real): Result; forward;
function str_result(r: Result): string; forward;
function makeResult(name: string; value: real): Result;
begin
  Result.name := name;
  Result.value := value;
end;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function pow10(n: integer): real;
var
  pow10_p: real;
  pow10_i: integer;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round_to(x: real; n: integer): real;
var
  round_to_m: real;
begin
  round_to_m := pow10(n);
  exit(Floor((x * round_to_m) + 0.5) / round_to_m);
end;
function electric_power(voltage: real; current: real; power: real): Result;
var
  electric_power_zeros: integer;
  electric_power_p: real;
begin
  electric_power_zeros := 0;
  if voltage = 0 then begin
  electric_power_zeros := electric_power_zeros + 1;
end;
  if current = 0 then begin
  electric_power_zeros := electric_power_zeros + 1;
end;
  if power = 0 then begin
  electric_power_zeros := electric_power_zeros + 1;
end;
  if electric_power_zeros <> 1 then begin
  panic('Exactly one argument must be 0');
end else begin
  if power < 0 then begin
  panic('Power cannot be negative in any electrical/electronics system');
end else begin
  if voltage = 0 then begin
  exit(makeResult('voltage', power / current));
end else begin
  if current = 0 then begin
  exit(makeResult('current', power / voltage));
end else begin
  if power = 0 then begin
  electric_power_p := absf(voltage * current);
  exit(makeResult('power', round_to(electric_power_p, 2)));
end else begin
  panic('Unhandled case');
end;
end;
end;
end;
end;
end;
function str_result(r: Result): string;
begin
  exit(((('Result(name=''' + r.name) + ''', value=') + FloatToStr(r.value)) + ')');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  r1 := electric_power(0, 2, 5);
  writeln(str_result(r1));
  r2 := electric_power(2, 2, 0);
  writeln(str_result(r2));
  r3 := electric_power(-2, 3, 0);
  writeln(str_result(r3));
  r4 := electric_power(2.2, 2.2, 0);
  writeln(str_result(r4));
  r5 := electric_power(2, 0, 6);
  writeln(str_result(r5));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
