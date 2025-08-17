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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: integer;
  b: integer;
  m: integer;
  n: integer;
  x: integer;
  y: integer;
function abs_int(x: integer): integer; forward;
function gcd(a: integer; b: integer): integer; forward;
function power(x: integer; y: integer; m: integer): integer; forward;
function is_carmichael_number(n: integer): boolean; forward;
function abs_int(x: integer): integer;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function gcd(a: integer; b: integer): integer;
begin
  if a = 0 then begin
  exit(abs_int(b));
end;
  exit(gcd(b mod a, a));
end;
function power(x: integer; y: integer; m: integer): integer;
var
  power_temp: integer;
begin
  if y = 0 then begin
  exit(1 mod m);
end;
  power_temp := power(x, y div 2, m) mod m;
  power_temp := (power_temp * power_temp) mod m;
  if (y mod 2) = 1 then begin
  power_temp := (power_temp * x) mod m;
end;
  exit(power_temp);
end;
function is_carmichael_number(n: integer): boolean;
var
  is_carmichael_number_b: integer;
begin
  if n <= 0 then begin
  panic('Number must be positive');
end;
  is_carmichael_number_b := 2;
  while is_carmichael_number_b < n do begin
  if gcd(is_carmichael_number_b, n) = 1 then begin
  if power(is_carmichael_number_b, n - 1, n) <> 1 then begin
  exit(false);
end;
end;
  is_carmichael_number_b := is_carmichael_number_b + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(power(2, 15, 3)));
  writeln(IntToStr(power(5, 1, 30)));
  writeln(LowerCase(BoolToStr(is_carmichael_number(4), true)));
  writeln(LowerCase(BoolToStr(is_carmichael_number(561), true)));
  writeln(LowerCase(BoolToStr(is_carmichael_number(562), true)));
  writeln(LowerCase(BoolToStr(is_carmichael_number(1105), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
