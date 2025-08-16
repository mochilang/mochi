{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type EuclidResult = record
  x: integer;
  y: integer;
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
  e1: EuclidResult;
  e2: EuclidResult;
  b: integer;
  n: integer;
  a: integer;
  n2: integer;
  r1: integer;
  r2: integer;
  n1: integer;
function makeEuclidResult(x: integer; y: integer): EuclidResult; forward;
function extended_euclid(a: integer; b: integer): EuclidResult; forward;
function chinese_remainder_theorem(n1: integer; r1: integer; n2: integer; r2: integer): integer; forward;
function invert_modulo(a: integer; n: integer): integer; forward;
function chinese_remainder_theorem2(n1: integer; r1: integer; n2: integer; r2: integer): integer; forward;
function makeEuclidResult(x: integer; y: integer): EuclidResult;
begin
  Result.x := x;
  Result.y := y;
end;
function extended_euclid(a: integer; b: integer): EuclidResult;
var
  extended_euclid_res: EuclidResult;
  extended_euclid_k: integer;
begin
  if b = 0 then begin
  exit(makeEuclidResult(1, 0));
end;
  extended_euclid_res := extended_euclid(b, a mod b);
  extended_euclid_k := a div b;
  exit(makeEuclidResult(extended_euclid_res.y, extended_euclid_res.x - (extended_euclid_k * extended_euclid_res.y)));
end;
function chinese_remainder_theorem(n1: integer; r1: integer; n2: integer; r2: integer): integer;
var
  chinese_remainder_theorem_res: EuclidResult;
  chinese_remainder_theorem_x: integer;
  chinese_remainder_theorem_y: integer;
  chinese_remainder_theorem_m: integer;
  chinese_remainder_theorem_n: integer;
begin
  chinese_remainder_theorem_res := extended_euclid(n1, n2);
  chinese_remainder_theorem_x := chinese_remainder_theorem_res.x;
  chinese_remainder_theorem_y := chinese_remainder_theorem_res.y;
  chinese_remainder_theorem_m := n1 * n2;
  chinese_remainder_theorem_n := ((r2 * chinese_remainder_theorem_x) * n1) + ((r1 * chinese_remainder_theorem_y) * n2);
  exit(((chinese_remainder_theorem_n mod chinese_remainder_theorem_m) + chinese_remainder_theorem_m) mod chinese_remainder_theorem_m);
end;
function invert_modulo(a: integer; n: integer): integer;
var
  invert_modulo_res: EuclidResult;
  invert_modulo_b: integer;
begin
  invert_modulo_res := extended_euclid(a, n);
  invert_modulo_b := invert_modulo_res.x;
  if invert_modulo_b < 0 then begin
  invert_modulo_b := ((invert_modulo_b mod n) + n) mod n;
end;
  exit(invert_modulo_b);
end;
function chinese_remainder_theorem2(n1: integer; r1: integer; n2: integer; r2: integer): integer;
var
  chinese_remainder_theorem2_x: integer;
  chinese_remainder_theorem2_y: integer;
  chinese_remainder_theorem2_m: integer;
  chinese_remainder_theorem2_n: integer;
begin
  chinese_remainder_theorem2_x := invert_modulo(n1, n2);
  chinese_remainder_theorem2_y := invert_modulo(n2, n1);
  chinese_remainder_theorem2_m := n1 * n2;
  chinese_remainder_theorem2_n := ((r2 * chinese_remainder_theorem2_x) * n1) + ((r1 * chinese_remainder_theorem2_y) * n2);
  exit(((chinese_remainder_theorem2_n mod chinese_remainder_theorem2_m) + chinese_remainder_theorem2_m) mod chinese_remainder_theorem2_m);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  e1 := extended_euclid(10, 6);
  writeln((IntToStr(e1.x) + ',') + IntToStr(e1.y));
  e2 := extended_euclid(7, 5);
  writeln((IntToStr(e2.x) + ',') + IntToStr(e2.y));
  writeln(IntToStr(chinese_remainder_theorem(5, 1, 7, 3)));
  writeln(IntToStr(chinese_remainder_theorem(6, 1, 4, 3)));
  writeln(IntToStr(invert_modulo(2, 5)));
  writeln(IntToStr(invert_modulo(8, 7)));
  writeln(IntToStr(chinese_remainder_theorem2(5, 1, 7, 3)));
  writeln(IntToStr(chinese_remainder_theorem2(6, 1, 4, 3)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
