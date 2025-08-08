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
  c: real;
  k: real;
  x: real;
  f: real;
  n: integer;
  ndigits: integer;
  r: real;
function floor(x: real): real; forward;
function pow10(n: integer): real; forward;
function round_to(x: real; ndigits: integer): real; forward;
function celsius_to_fahrenheit(c: real; ndigits: integer): real; forward;
function celsius_to_kelvin(c: real; ndigits: integer): real; forward;
function celsius_to_rankine(c: real; ndigits: integer): real; forward;
function fahrenheit_to_celsius(f: real; ndigits: integer): real; forward;
function fahrenheit_to_kelvin(f: real; ndigits: integer): real; forward;
function fahrenheit_to_rankine(f: real; ndigits: integer): real; forward;
function kelvin_to_celsius(k: real; ndigits: integer): real; forward;
function kelvin_to_fahrenheit(k: real; ndigits: integer): real; forward;
function kelvin_to_rankine(k: real; ndigits: integer): real; forward;
function rankine_to_celsius(r: real; ndigits: integer): real; forward;
function rankine_to_fahrenheit(r: real; ndigits: integer): real; forward;
function rankine_to_kelvin(r: real; ndigits: integer): real; forward;
function reaumur_to_kelvin(r: real; ndigits: integer): real; forward;
function reaumur_to_fahrenheit(r: real; ndigits: integer): real; forward;
function reaumur_to_celsius(r: real; ndigits: integer): real; forward;
function reaumur_to_rankine(r: real; ndigits: integer): real; forward;
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
function round_to(x: real; ndigits: integer): real;
var
  round_to_m: real;
begin
  round_to_m := pow10(ndigits);
  exit(floor((x * round_to_m) + 0.5) / round_to_m);
end;
function celsius_to_fahrenheit(c: real; ndigits: integer): real;
begin
  exit(round_to(((c * 9) / 5) + 32, ndigits));
end;
function celsius_to_kelvin(c: real; ndigits: integer): real;
begin
  exit(round_to(c + 273.15, ndigits));
end;
function celsius_to_rankine(c: real; ndigits: integer): real;
begin
  exit(round_to(((c * 9) / 5) + 491.67, ndigits));
end;
function fahrenheit_to_celsius(f: real; ndigits: integer): real;
begin
  exit(round_to(((f - 32) * 5) / 9, ndigits));
end;
function fahrenheit_to_kelvin(f: real; ndigits: integer): real;
begin
  exit(round_to((((f - 32) * 5) / 9) + 273.15, ndigits));
end;
function fahrenheit_to_rankine(f: real; ndigits: integer): real;
begin
  exit(round_to(f + 459.67, ndigits));
end;
function kelvin_to_celsius(k: real; ndigits: integer): real;
begin
  exit(round_to(k - 273.15, ndigits));
end;
function kelvin_to_fahrenheit(k: real; ndigits: integer): real;
begin
  exit(round_to((((k - 273.15) * 9) / 5) + 32, ndigits));
end;
function kelvin_to_rankine(k: real; ndigits: integer): real;
begin
  exit(round_to((k * 9) / 5, ndigits));
end;
function rankine_to_celsius(r: real; ndigits: integer): real;
begin
  exit(round_to(((r - 491.67) * 5) / 9, ndigits));
end;
function rankine_to_fahrenheit(r: real; ndigits: integer): real;
begin
  exit(round_to(r - 459.67, ndigits));
end;
function rankine_to_kelvin(r: real; ndigits: integer): real;
begin
  exit(round_to((r * 5) / 9, ndigits));
end;
function reaumur_to_kelvin(r: real; ndigits: integer): real;
begin
  exit(round_to((r * 1.25) + 273.15, ndigits));
end;
function reaumur_to_fahrenheit(r: real; ndigits: integer): real;
begin
  exit(round_to((r * 2.25) + 32, ndigits));
end;
function reaumur_to_celsius(r: real; ndigits: integer): real;
begin
  exit(round_to(r * 1.25, ndigits));
end;
function reaumur_to_rankine(r: real; ndigits: integer): real;
begin
  exit(round_to(((r * 2.25) + 32) + 459.67, ndigits));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(celsius_to_fahrenheit(0, 2));
  writeln(celsius_to_kelvin(0, 2));
  writeln(celsius_to_rankine(0, 2));
  writeln(fahrenheit_to_celsius(32, 2));
  writeln(fahrenheit_to_kelvin(32, 2));
  writeln(fahrenheit_to_rankine(32, 2));
  writeln(kelvin_to_celsius(273.15, 2));
  writeln(kelvin_to_fahrenheit(273.15, 2));
  writeln(kelvin_to_rankine(273.15, 2));
  writeln(rankine_to_celsius(491.67, 2));
  writeln(rankine_to_fahrenheit(491.67, 2));
  writeln(rankine_to_kelvin(491.67, 2));
  writeln(reaumur_to_kelvin(80, 2));
  writeln(reaumur_to_fahrenheit(80, 2));
  writeln(reaumur_to_celsius(80, 2));
  writeln(reaumur_to_rankine(80, 2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
