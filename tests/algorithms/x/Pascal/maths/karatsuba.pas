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
  base: integer;
  a: integer;
  b: integer;
  exp: integer;
function int_pow(base: integer; exp: integer): integer; forward;
function karatsuba(a: integer; b: integer): integer; forward;
procedure main(); forward;
function int_pow(base: integer; exp: integer): integer;
var
  int_pow_result_: integer;
  int_pow_i: integer;
begin
  int_pow_result_ := 1;
  int_pow_i := 0;
  while int_pow_i < exp do begin
  int_pow_result_ := int_pow_result_ * base;
  int_pow_i := int_pow_i + 1;
end;
  exit(int_pow_result_);
end;
function karatsuba(a: integer; b: integer): integer;
var
  karatsuba_m1: integer;
  karatsuba_lb: integer;
  karatsuba_m2: integer;
  karatsuba_power: integer;
  karatsuba_a1: integer;
  karatsuba_a2: integer;
  karatsuba_b1: integer;
  karatsuba_b2: integer;
  karatsuba_x: integer;
  karatsuba_y: integer;
  karatsuba_z: integer;
  karatsuba_result_: integer;
begin
  if (Length(IntToStr(a)) = 1) or (Length(IntToStr(b)) = 1) then begin
  exit(a * b);
end;
  karatsuba_m1 := Length(IntToStr(a));
  karatsuba_lb := Length(IntToStr(b));
  if karatsuba_lb > karatsuba_m1 then begin
  karatsuba_m1 := karatsuba_lb;
end;
  karatsuba_m2 := karatsuba_m1 div 2;
  karatsuba_power := int_pow(10, karatsuba_m2);
  karatsuba_a1 := a div karatsuba_power;
  karatsuba_a2 := a mod karatsuba_power;
  karatsuba_b1 := b div karatsuba_power;
  karatsuba_b2 := b mod karatsuba_power;
  karatsuba_x := karatsuba(karatsuba_a2, karatsuba_b2);
  karatsuba_y := karatsuba(karatsuba_a1 + karatsuba_a2, karatsuba_b1 + karatsuba_b2);
  karatsuba_z := karatsuba(karatsuba_a1, karatsuba_b1);
  karatsuba_result_ := ((karatsuba_z * int_pow(10, 2 * karatsuba_m2)) + (((karatsuba_y - karatsuba_z) - karatsuba_x) * karatsuba_power)) + karatsuba_x;
  exit(karatsuba_result_);
end;
procedure main();
begin
  writeln(IntToStr(karatsuba(15463, 23489)));
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
