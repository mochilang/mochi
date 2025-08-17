{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: IntArray;
  b: IntArray;
  n: integer;
function prime_factors(n: integer): IntArray; forward;
function list_eq(a: IntArray; b: IntArray): boolean; forward;
procedure test_prime_factors(); forward;
procedure main(); forward;
function prime_factors(n: integer): IntArray;
var
  prime_factors_num: integer;
  prime_factors_i: integer;
  prime_factors_factors: array of integer;
begin
  if n < 2 then begin
  exit([]);
end;
  prime_factors_num := n;
  prime_factors_i := 2;
  prime_factors_factors := [];
  while (prime_factors_i * prime_factors_i) <= prime_factors_num do begin
  if (prime_factors_num mod prime_factors_i) = 0 then begin
  prime_factors_factors := concat(prime_factors_factors, IntArray([prime_factors_i]));
  prime_factors_num := prime_factors_num div prime_factors_i;
end else begin
  prime_factors_i := prime_factors_i + 1;
end;
end;
  if prime_factors_num > 1 then begin
  prime_factors_factors := concat(prime_factors_factors, IntArray([prime_factors_num]));
end;
  exit(prime_factors_factors);
end;
function list_eq(a: IntArray; b: IntArray): boolean;
var
  list_eq_i: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  list_eq_i := 0;
  while list_eq_i < Length(a) do begin
  if a[list_eq_i] <> b[list_eq_i] then begin
  exit(false);
end;
  list_eq_i := list_eq_i + 1;
end;
  exit(true);
end;
procedure test_prime_factors();
begin
  if not list_eq(prime_factors(0), []) then begin
  panic('prime_factors(0) failed');
end;
  if not list_eq(prime_factors(100), [2, 2, 5, 5]) then begin
  panic('prime_factors(100) failed');
end;
  if not list_eq(prime_factors(2560), [2, 2, 2, 2, 2, 2, 2, 2, 2, 5]) then begin
  panic('prime_factors(2560) failed');
end;
  if not list_eq(prime_factors(97), [97]) then begin
  panic('prime_factors(97) failed');
end;
end;
procedure main();
begin
  test_prime_factors();
  writeln(list_int_to_str(prime_factors(100)));
  writeln(list_int_to_str(prime_factors(2560)));
  writeln(list_int_to_str(prime_factors(97)));
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
