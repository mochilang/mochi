{$mode objfpc}
program Main;
uses SysUtils, Math;
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
  number: real;
  x: real;
  n: integer;
  digit_amount: integer;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round(x: real; n: integer): real; forward;
function decimal_isolate(number: real; digit_amount: integer): real; forward;
procedure main(); forward;
function floor(x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(x);
  if Double(floor_i) > x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
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
function round(x: real; n: integer): real;
var
  round_m: real;
begin
  round_m := pow10(n);
  exit(Floor((x * round_m) + 0.5) / round_m);
end;
function decimal_isolate(number: real; digit_amount: integer): real;
var
  decimal_isolate_whole: integer;
  decimal_isolate_frac: real;
begin
  decimal_isolate_whole := Trunc(number);
  decimal_isolate_frac := number - Double(decimal_isolate_whole);
  if digit_amount > 0 then begin
  exit(round(decimal_isolate_frac, digit_amount));
end;
  exit(decimal_isolate_frac);
end;
procedure main();
begin
  writeln(FloatToStr(decimal_isolate(1.53, 0)));
  writeln(FloatToStr(decimal_isolate(35.345, 1)));
  writeln(FloatToStr(decimal_isolate(35.345, 2)));
  writeln(FloatToStr(decimal_isolate(35.345, 3)));
  writeln(FloatToStr(decimal_isolate(-14.789, 3)));
  writeln(FloatToStr(decimal_isolate(0, 2)));
  writeln(FloatToStr(decimal_isolate(-14.123, 1)));
  writeln(FloatToStr(decimal_isolate(-14.123, 2)));
  writeln(FloatToStr(decimal_isolate(-14.123, 3)));
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
