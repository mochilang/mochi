{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  xs: IntArray;
  idx: integer;
  k: integer;
  n: integer;
  a: IntArray;
  b: IntArray;
function remove_at(xs: IntArray; idx: integer): IntArray; forward;
function kth_permutation(k: integer; n: integer): IntArray; forward;
function list_equal(a: IntArray; b: IntArray): boolean; forward;
function list_to_string(xs: IntArray): string; forward;
procedure test_kth_permutation(); forward;
procedure main(); forward;
function remove_at(xs: IntArray; idx: integer): IntArray;
var
  remove_at_res: array of integer;
  remove_at_i: integer;
begin
  remove_at_res := [];
  remove_at_i := 0;
  while remove_at_i < Length(xs) do begin
  if remove_at_i <> idx then begin
  remove_at_res := concat(remove_at_res, IntArray([xs[remove_at_i]]));
end;
  remove_at_i := remove_at_i + 1;
end;
  exit(remove_at_res);
end;
function kth_permutation(k: integer; n: integer): IntArray;
var
  kth_permutation_factorials: array of integer;
  kth_permutation_i: integer;
  kth_permutation_total: integer;
  kth_permutation_elements: array of integer;
  kth_permutation_e: integer;
  kth_permutation_permutation: array of integer;
  kth_permutation_idx: integer;
  kth_permutation_factorial: integer;
  kth_permutation_number: integer;
begin
  if n <= 0 then begin
  panic('n must be positive');
end;
  kth_permutation_factorials := [1];
  kth_permutation_i := 2;
  while kth_permutation_i < n do begin
  kth_permutation_factorials := concat(kth_permutation_factorials, IntArray([kth_permutation_factorials[Length(kth_permutation_factorials) - 1] * kth_permutation_i]));
  kth_permutation_i := kth_permutation_i + 1;
end;
  kth_permutation_total := kth_permutation_factorials[Length(kth_permutation_factorials) - 1] * n;
  if (k < 0) or (k >= kth_permutation_total) then begin
  panic('k out of bounds');
end;
  kth_permutation_elements := [];
  kth_permutation_e := 0;
  while kth_permutation_e < n do begin
  kth_permutation_elements := concat(kth_permutation_elements, IntArray([kth_permutation_e]));
  kth_permutation_e := kth_permutation_e + 1;
end;
  kth_permutation_permutation := [];
  kth_permutation_idx := Length(kth_permutation_factorials) - 1;
  while kth_permutation_idx >= 0 do begin
  kth_permutation_factorial := kth_permutation_factorials[kth_permutation_idx];
  kth_permutation_number := k div kth_permutation_factorial;
  k := k mod kth_permutation_factorial;
  kth_permutation_permutation := concat(kth_permutation_permutation, IntArray([kth_permutation_elements[kth_permutation_number]]));
  kth_permutation_elements := remove_at(kth_permutation_elements, kth_permutation_number);
  kth_permutation_idx := kth_permutation_idx - 1;
end;
  kth_permutation_permutation := concat(kth_permutation_permutation, IntArray([kth_permutation_elements[0]]));
  exit(kth_permutation_permutation);
end;
function list_equal(a: IntArray; b: IntArray): boolean;
var
  list_equal_i: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  list_equal_i := 0;
  while list_equal_i < Length(a) do begin
  if a[list_equal_i] <> b[list_equal_i] then begin
  exit(false);
end;
  list_equal_i := list_equal_i + 1;
end;
  exit(true);
end;
function list_to_string(xs: IntArray): string;
var
  list_to_string_s: string;
  list_to_string_i: integer;
begin
  if Length(xs) = 0 then begin
  exit('[]');
end;
  list_to_string_s := '[' + IntToStr(xs[0]);
  list_to_string_i := 1;
  while list_to_string_i < Length(xs) do begin
  list_to_string_s := (list_to_string_s + ', ') + IntToStr(xs[list_to_string_i]);
  list_to_string_i := list_to_string_i + 1;
end;
  list_to_string_s := list_to_string_s + ']';
  exit(list_to_string_s);
end;
procedure test_kth_permutation();
var
  test_kth_permutation_expected1: array of integer;
  test_kth_permutation_res1: IntArray;
  test_kth_permutation_expected2: array of integer;
  test_kth_permutation_res2: IntArray;
begin
  test_kth_permutation_expected1 := [0, 1, 2, 3, 4];
  test_kth_permutation_res1 := kth_permutation(0, 5);
  if not list_equal(test_kth_permutation_res1, test_kth_permutation_expected1) then begin
  panic('test case 1 failed');
end;
  test_kth_permutation_expected2 := [1, 3, 0, 2];
  test_kth_permutation_res2 := kth_permutation(10, 4);
  if not list_equal(test_kth_permutation_res2, test_kth_permutation_expected2) then begin
  panic('test case 2 failed');
end;
end;
procedure main();
var
  main_res: IntArray;
begin
  test_kth_permutation();
  main_res := kth_permutation(10, 4);
  writeln(list_to_string(main_res));
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
