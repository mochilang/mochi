{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type Result_ = record
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
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  r1: Result_;
  r2: Result_;
  r3: Result_;
  r4: Result_;
  r5: Result_;
function makeResult_(name: string; value: real): Result_; forward;
function absf(absf_x: real): real; forward;
function pow10(pow10_n: int64): real; forward;
function round_to(round_to_x: real; round_to_n: int64): real; forward;
function electric_power(electric_power_voltage: real; electric_power_current: real; electric_power_power: real): Result_; forward;
function str_result(str_result_r: Result_): string; forward;
function makeResult_(name: string; value: real): Result_;
begin
  Result.name := name;
  Result.value := value;
end;
function absf(absf_x: real): real;
begin
  if absf_x < 0 then begin
  exit(-absf_x);
end;
  exit(absf_x);
end;
function pow10(pow10_n: int64): real;
var
  pow10_p: real;
  pow10_i: int64;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function round_to(round_to_x: real; round_to_n: int64): real;
var
  round_to_m: real;
begin
  round_to_m := pow10(round_to_n);
  exit(Floor((round_to_x * round_to_m) + 0.5) / round_to_m);
end;
function electric_power(electric_power_voltage: real; electric_power_current: real; electric_power_power: real): Result_;
var
  electric_power_zeros: int64;
  electric_power_p: real;
begin
  electric_power_zeros := 0;
  if electric_power_voltage = 0 then begin
  electric_power_zeros := electric_power_zeros + 1;
end;
  if electric_power_current = 0 then begin
  electric_power_zeros := electric_power_zeros + 1;
end;
  if electric_power_power = 0 then begin
  electric_power_zeros := electric_power_zeros + 1;
end;
  if electric_power_zeros <> 1 then begin
  panic('Exactly one argument must be 0');
end else begin
  if electric_power_power < 0 then begin
  panic('Power cannot be negative in any electrical/electronics system');
end else begin
  if electric_power_voltage = 0 then begin
  exit(makeResult_('voltage', electric_power_power / electric_power_current));
end else begin
  if electric_power_current = 0 then begin
  exit(makeResult_('current', electric_power_power / electric_power_voltage));
end else begin
  if electric_power_power = 0 then begin
  electric_power_p := absf(electric_power_voltage * electric_power_current);
  exit(makeResult_('power', round_to(electric_power_p, 2)));
end else begin
  panic('Unhandled case');
end;
end;
end;
end;
end;
end;
function str_result(str_result_r: Result_): string;
begin
  exit(((('Result(name=''' + str_result_r.name) + ''', value=') + FloatToStr(str_result_r.value)) + ')');
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
  writeln('');
end.
