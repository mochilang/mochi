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
function min_int(a: integer; b: integer): integer; forward;
function int_sqrt(n: integer): integer; forward;
function sieve(n: integer): IntArray; forward;
function lists_equal(a: IntArray; b: IntArray): boolean; forward;
procedure test_sieve(); forward;
procedure main(); forward;
function min_int(a: integer; b: integer): integer;
begin
  if a < b then begin
  exit(a);
end;
  exit(b);
end;
function int_sqrt(n: integer): integer;
var
  int_sqrt_r: integer;
begin
  int_sqrt_r := 0;
  while ((int_sqrt_r + 1) * (int_sqrt_r + 1)) <= n do begin
  int_sqrt_r := int_sqrt_r + 1;
end;
  exit(int_sqrt_r);
end;
function sieve(n: integer): IntArray;
var
  sieve_in_prime: array of integer;
  sieve_start: integer;
  sieve_end_: integer;
  sieve_temp: array of integer;
  sieve_i: integer;
  sieve_prime: array of integer;
  sieve_j: integer;
  sieve_low: integer;
  sieve_high: integer;
  sieve_tempSeg: array of integer;
  sieve_size: integer;
  sieve_k: integer;
  sieve_idx: integer;
  sieve_each: integer;
  sieve_t: integer;
  sieve_j2: integer;
  sieve_j3: integer;
begin
  if n <= 0 then begin
  panic('Number must instead be a positive integer');
end;
  sieve_in_prime := [];
  sieve_start := 2;
  sieve_end_ := int_sqrt(n);
  sieve_temp := [];
  sieve_i := 0;
  while sieve_i < (sieve_end_ + 1) do begin
  sieve_temp := concat(sieve_temp, IntArray([1]));
  sieve_i := sieve_i + 1;
end;
  sieve_prime := [];
  while sieve_start <= sieve_end_ do begin
  if sieve_temp[sieve_start] = 1 then begin
  sieve_in_prime := concat(sieve_in_prime, IntArray([sieve_start]));
  sieve_j := sieve_start * sieve_start;
  while sieve_j <= sieve_end_ do begin
  sieve_temp[sieve_j] := 0;
  sieve_j := sieve_j + sieve_start;
end;
end;
  sieve_start := sieve_start + 1;
end;
  sieve_i := 0;
  while sieve_i < Length(sieve_in_prime) do begin
  sieve_prime := concat(sieve_prime, IntArray([sieve_in_prime[sieve_i]]));
  sieve_i := sieve_i + 1;
end;
  sieve_low := sieve_end_ + 1;
  sieve_high := min_int(2 * sieve_end_, n);
  while sieve_low <= n do begin
  sieve_tempSeg := [];
  sieve_size := (sieve_high - sieve_low) + 1;
  sieve_k := 0;
  while sieve_k < sieve_size do begin
  sieve_tempSeg := concat(sieve_tempSeg, IntArray([1]));
  sieve_k := sieve_k + 1;
end;
  sieve_idx := 0;
  while sieve_idx < Length(sieve_in_prime) do begin
  sieve_each := sieve_in_prime[sieve_idx];
  sieve_t := (sieve_low div sieve_each) * sieve_each;
  if sieve_t < sieve_low then begin
  sieve_t := sieve_t + sieve_each;
end;
  sieve_j2 := sieve_t;
  while sieve_j2 <= sieve_high do begin
  sieve_tempSeg[sieve_j2 - sieve_low] := 0;
  sieve_j2 := sieve_j2 + sieve_each;
end;
  sieve_idx := sieve_idx + 1;
end;
  sieve_j3 := 0;
  while sieve_j3 < Length(sieve_tempSeg) do begin
  if sieve_tempSeg[sieve_j3] = 1 then begin
  sieve_prime := concat(sieve_prime, IntArray([sieve_j3 + sieve_low]));
end;
  sieve_j3 := sieve_j3 + 1;
end;
  sieve_low := sieve_high + 1;
  sieve_high := min_int(sieve_high + sieve_end_, n);
end;
  exit(sieve_prime);
end;
function lists_equal(a: IntArray; b: IntArray): boolean;
var
  lists_equal_m: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  lists_equal_m := 0;
  while lists_equal_m < Length(a) do begin
  if a[lists_equal_m] <> b[lists_equal_m] then begin
  exit(false);
end;
  lists_equal_m := lists_equal_m + 1;
end;
  exit(true);
end;
procedure test_sieve();
var
  test_sieve_e1: IntArray;
  test_sieve_e2: IntArray;
begin
  test_sieve_e1 := sieve(8);
  if not lists_equal(test_sieve_e1, [2, 3, 5, 7]) then begin
  panic('sieve(8) failed');
end;
  test_sieve_e2 := sieve(27);
  if not lists_equal(test_sieve_e2, [2, 3, 5, 7, 11, 13, 17, 19, 23]) then begin
  panic('sieve(27) failed');
end;
end;
procedure main();
begin
  test_sieve();
  writeln(list_int_to_str(sieve(30)));
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
