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
  x: integer;
  y: integer;
  n: integer;
  a: integer;
  b: integer;
function abs_int(n: integer): integer; forward;
function greatest_common_divisor(a: integer; b: integer): integer; forward;
function gcd_by_iterative(x: integer; y: integer): integer; forward;
function abs_int(n: integer): integer;
begin
  if n < 0 then begin
  exit(-n);
end;
  exit(n);
end;
function greatest_common_divisor(a: integer; b: integer): integer;
var
  greatest_common_divisor_x: integer;
  greatest_common_divisor_y: integer;
begin
  greatest_common_divisor_x := abs_int(a);
  greatest_common_divisor_y := abs_int(b);
  if greatest_common_divisor_x = 0 then begin
  exit(greatest_common_divisor_y);
end;
  exit(greatest_common_divisor(greatest_common_divisor_y mod greatest_common_divisor_x, greatest_common_divisor_x));
end;
function gcd_by_iterative(x: integer; y: integer): integer;
var
  gcd_by_iterative_a: integer;
  gcd_by_iterative_b: integer;
  gcd_by_iterative_temp: integer;
begin
  gcd_by_iterative_a := abs_int(x);
  gcd_by_iterative_b := abs_int(y);
  while gcd_by_iterative_b <> 0 do begin
  gcd_by_iterative_temp := gcd_by_iterative_b;
  gcd_by_iterative_b := gcd_by_iterative_a mod gcd_by_iterative_b;
  gcd_by_iterative_a := gcd_by_iterative_temp;
end;
  exit(gcd_by_iterative_a);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(greatest_common_divisor(24, 40)));
  writeln(IntToStr(greatest_common_divisor(1, 1)));
  writeln(IntToStr(greatest_common_divisor(1, 800)));
  writeln(IntToStr(greatest_common_divisor(11, 37)));
  writeln(IntToStr(greatest_common_divisor(3, 5)));
  writeln(IntToStr(greatest_common_divisor(16, 4)));
  writeln(IntToStr(greatest_common_divisor(-3, 9)));
  writeln(IntToStr(greatest_common_divisor(9, -3)));
  writeln(IntToStr(greatest_common_divisor(3, -9)));
  writeln(IntToStr(greatest_common_divisor(-3, -9)));
  writeln(IntToStr(gcd_by_iterative(24, 40)));
  writeln(LowerCase(BoolToStr(greatest_common_divisor(24, 40) = gcd_by_iterative(24, 40), true)));
  writeln(IntToStr(gcd_by_iterative(-3, -9)));
  writeln(IntToStr(gcd_by_iterative(3, -9)));
  writeln(IntToStr(gcd_by_iterative(1, -800)));
  writeln(IntToStr(gcd_by_iterative(11, 37)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
