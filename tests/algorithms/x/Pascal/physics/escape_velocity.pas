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
  x: real;
  mass: real;
  radius: real;
  n: real;
function pow10(n: integer): real; forward;
function sqrt_newton(n: real): real; forward;
function round3(x: real): real; forward;
function escape_velocity(mass: real; radius: real): real; forward;
function pow10(n: integer): real;
var
  pow10_p: real;
  pow10_k: integer;
  pow10_m: integer;
begin
  pow10_p := 1;
  pow10_k := 0;
  if n >= 0 then begin
  while pow10_k < n do begin
  pow10_p := pow10_p * 10;
  pow10_k := pow10_k + 1;
end;
end else begin
  pow10_m := -n;
  while pow10_k < pow10_m do begin
  pow10_p := pow10_p / 10;
  pow10_k := pow10_k + 1;
end;
end;
  exit(pow10_p);
end;
function sqrt_newton(n: real): real;
var
  sqrt_newton_x: real;
  sqrt_newton_j: integer;
begin
  if n = 0 then begin
  exit(0);
end;
  sqrt_newton_x := n;
  sqrt_newton_j := 0;
  while sqrt_newton_j < 20 do begin
  sqrt_newton_x := (sqrt_newton_x + (n / sqrt_newton_x)) / 2;
  sqrt_newton_j := sqrt_newton_j + 1;
end;
  exit(sqrt_newton_x);
end;
function round3(x: real): real;
var
  round3_y: real;
  round3_yi: integer;
begin
  round3_y := (x * 1000) + 0.5;
  round3_yi := Trunc(round3_y);
  if Double(round3_yi) > round3_y then begin
  round3_yi := round3_yi - 1;
end;
  exit(Double(round3_yi) / 1000);
end;
function escape_velocity(mass: real; radius: real): real;
var
  escape_velocity_G: real;
  escape_velocity_velocity: real;
begin
  if radius = 0 then begin
  panic('Radius cannot be zero.');
end;
  escape_velocity_G := 6.6743 * pow10(-11);
  escape_velocity_velocity := sqrt_newton(((2 * escape_velocity_G) * mass) / radius);
  exit(round3(escape_velocity_velocity));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(escape_velocity(5.972 * pow10(24), 6.371 * pow10(6)));
  writeln(escape_velocity(7.348 * pow10(22), 1.737 * pow10(6)));
  writeln(escape_velocity(1.898 * pow10(27), 6.9911 * pow10(7)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
