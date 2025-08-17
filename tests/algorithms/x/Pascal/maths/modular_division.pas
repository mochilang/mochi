{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of integer;
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
  n: integer;
function mod_(a: integer; n: integer): integer; forward;
function greatest_common_divisor(a: integer; b: integer): integer; forward;
function extended_gcd(a: integer; b: integer): IntArray; forward;
function extended_euclid(a: integer; b: integer): IntArray; forward;
function invert_modulo(a: integer; n: integer): integer; forward;
function modular_division(a: integer; b: integer; n: integer): integer; forward;
function modular_division2(a: integer; b: integer; n: integer): integer; forward;
procedure tests(); forward;
procedure main(); forward;
function mod_(a: integer; n: integer): integer;
var
  mod__r: integer;
begin
  mod__r := a mod n;
  if mod__r < 0 then begin
  exit(mod__r + n);
end;
  exit(mod__r);
end;
function greatest_common_divisor(a: integer; b: integer): integer;
var
  greatest_common_divisor_x: integer;
  greatest_common_divisor_y: integer;
  greatest_common_divisor_t: integer;
begin
  greatest_common_divisor_x := IfThen(a < 0, -a, a);
  greatest_common_divisor_y := IfThen(b < 0, -b, b);
  while greatest_common_divisor_y <> 0 do begin
  greatest_common_divisor_t := greatest_common_divisor_x mod greatest_common_divisor_y;
  greatest_common_divisor_x := greatest_common_divisor_y;
  greatest_common_divisor_y := greatest_common_divisor_t;
end;
  exit(greatest_common_divisor_x);
end;
function extended_gcd(a: integer; b: integer): IntArray;
var
  extended_gcd_res: array of integer;
  extended_gcd_d: integer;
  extended_gcd_p: integer;
  extended_gcd_q: integer;
  extended_gcd_x: integer;
  extended_gcd_y: integer;
begin
  if b = 0 then begin
  exit([a, 1, 0]);
end;
  extended_gcd_res := extended_gcd(b, a mod b);
  extended_gcd_d := extended_gcd_res[0];
  extended_gcd_p := extended_gcd_res[1];
  extended_gcd_q := extended_gcd_res[2];
  extended_gcd_x := extended_gcd_q;
  extended_gcd_y := extended_gcd_p - (extended_gcd_q * (a div b));
  exit([extended_gcd_d, extended_gcd_x, extended_gcd_y]);
end;
function extended_euclid(a: integer; b: integer): IntArray;
var
  extended_euclid_res: array of integer;
  extended_euclid_x: integer;
  extended_euclid_y: integer;
begin
  if b = 0 then begin
  exit([1, 0]);
end;
  extended_euclid_res := extended_euclid(b, a mod b);
  extended_euclid_x := extended_euclid_res[1];
  extended_euclid_y := extended_euclid_res[0] - ((a div b) * extended_euclid_res[1]);
  exit([extended_euclid_x, extended_euclid_y]);
end;
function invert_modulo(a: integer; n: integer): integer;
var
  invert_modulo_res: IntArray;
  invert_modulo_inv: integer;
begin
  invert_modulo_res := extended_euclid(a, n);
  invert_modulo_inv := invert_modulo_res[0];
  exit(mod_(invert_modulo_inv, n));
end;
function modular_division(a: integer; b: integer; n: integer): integer;
var
  modular_division_eg: IntArray;
  modular_division_s: integer;
begin
  if n <= 1 then begin
  panic('n must be > 1');
end;
  if a <= 0 then begin
  panic('a must be > 0');
end;
  if greatest_common_divisor(a, n) <> 1 then begin
  panic('gcd(a,n) != 1');
end;
  modular_division_eg := extended_gcd(n, a);
  modular_division_s := modular_division_eg[2];
  exit(mod_(b * modular_division_s, n));
end;
function modular_division2(a: integer; b: integer; n: integer): integer;
var
  modular_division2_s: integer;
begin
  modular_division2_s := invert_modulo(a, n);
  exit(mod_(b * modular_division2_s, n));
end;
procedure tests();
var
  tests_eg: IntArray;
  tests_eu: IntArray;
begin
  if modular_division(4, 8, 5) <> 2 then begin
  panic('md1');
end;
  if modular_division(3, 8, 5) <> 1 then begin
  panic('md2');
end;
  if modular_division(4, 11, 5) <> 4 then begin
  panic('md3');
end;
  if modular_division2(4, 8, 5) <> 2 then begin
  panic('md21');
end;
  if modular_division2(3, 8, 5) <> 1 then begin
  panic('md22');
end;
  if modular_division2(4, 11, 5) <> 4 then begin
  panic('md23');
end;
  if invert_modulo(2, 5) <> 3 then begin
  panic('inv');
end;
  tests_eg := extended_gcd(10, 6);
  if ((tests_eg[0] <> 2) or (tests_eg[1] <> -1)) or (tests_eg[2] <> 2) then begin
  panic('eg');
end;
  tests_eu := extended_euclid(10, 6);
  if (tests_eu[0] <> -1) or (tests_eu[1] <> 2) then begin
  panic('eu');
end;
  if greatest_common_divisor(121, 11) <> 11 then begin
  panic('gcd');
end;
end;
procedure main();
begin
  tests();
  writeln(IntToStr(modular_division(4, 8, 5)));
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
