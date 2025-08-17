{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of integer;
type BoolArray = array of boolean;
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
  n: integer;
  num: integer;
  number: integer;
  xs: IntArray;
function bubble_sort(xs: IntArray): IntArray; forward;
function factors(num: integer): IntArray; forward;
function sum_list(xs: IntArray): integer; forward;
function abundant(n: integer): boolean; forward;
function semi_perfect(number: integer): boolean; forward;
function weird(number: integer): boolean; forward;
procedure run_tests(); forward;
procedure main(); forward;
function bubble_sort(xs: IntArray): IntArray;
var
  bubble_sort_arr: array of integer;
  bubble_sort_n: integer;
  bubble_sort_i: integer;
  bubble_sort_j: integer;
  bubble_sort_tmp: integer;
begin
  bubble_sort_arr := xs;
  bubble_sort_n := Length(bubble_sort_arr);
  bubble_sort_i := 0;
  while bubble_sort_i < bubble_sort_n do begin
  bubble_sort_j := 0;
  while bubble_sort_j < ((bubble_sort_n - bubble_sort_i) - 1) do begin
  if bubble_sort_arr[bubble_sort_j] > bubble_sort_arr[bubble_sort_j + 1] then begin
  bubble_sort_tmp := bubble_sort_arr[bubble_sort_j];
  bubble_sort_arr[bubble_sort_j] := bubble_sort_arr[bubble_sort_j + 1];
  bubble_sort_arr[bubble_sort_j + 1] := bubble_sort_tmp;
end;
  bubble_sort_j := bubble_sort_j + 1;
end;
  bubble_sort_i := bubble_sort_i + 1;
end;
  exit(bubble_sort_arr);
end;
function factors(num: integer): IntArray;
var
  factors_values: array of integer;
  factors_i: integer;
  factors_d: integer;
begin
  factors_values := [1];
  factors_i := 2;
  while (factors_i * factors_i) <= num do begin
  if (num mod factors_i) = 0 then begin
  factors_values := concat(factors_values, IntArray([factors_i]));
  factors_d := num div factors_i;
  if factors_d <> factors_i then begin
  factors_values := concat(factors_values, IntArray([factors_d]));
end;
end;
  factors_i := factors_i + 1;
end;
  exit(bubble_sort(factors_values));
end;
function sum_list(xs: IntArray): integer;
var
  sum_list_total: integer;
  sum_list_i: integer;
begin
  sum_list_total := 0;
  sum_list_i := 0;
  while sum_list_i < Length(xs) do begin
  sum_list_total := sum_list_total + xs[sum_list_i];
  sum_list_i := sum_list_i + 1;
end;
  exit(sum_list_total);
end;
function abundant(n: integer): boolean;
begin
  exit(sum_list(factors(n)) > n);
end;
function semi_perfect(number: integer): boolean;
var
  semi_perfect_values: IntArray;
  semi_perfect_possible: array of boolean;
  semi_perfect_j: integer;
  semi_perfect_idx: integer;
  semi_perfect_v: integer;
  semi_perfect_s: integer;
begin
  if number <= 0 then begin
  exit(true);
end;
  semi_perfect_values := factors(number);
  semi_perfect_possible := [];
  semi_perfect_j := 0;
  while semi_perfect_j <= number do begin
  semi_perfect_possible := concat(semi_perfect_possible, [semi_perfect_j = 0]);
  semi_perfect_j := semi_perfect_j + 1;
end;
  semi_perfect_idx := 0;
  while semi_perfect_idx < Length(semi_perfect_values) do begin
  semi_perfect_v := semi_perfect_values[semi_perfect_idx];
  semi_perfect_s := number;
  while semi_perfect_s >= semi_perfect_v do begin
  if semi_perfect_possible[semi_perfect_s - semi_perfect_v] then begin
  semi_perfect_possible[semi_perfect_s] := true;
end;
  semi_perfect_s := semi_perfect_s - 1;
end;
  semi_perfect_idx := semi_perfect_idx + 1;
end;
  exit(semi_perfect_possible[number]);
end;
function weird(number: integer): boolean;
begin
  exit(abundant(number) and (semi_perfect(number) = false));
end;
procedure run_tests();
begin
  if list_int_to_str(factors(12)) <> list_int_to_str([1, 2, 3, 4, 6]) then begin
  panic('factors 12 failed');
end;
  if list_int_to_str(factors(1)) <> list_int_to_str([1]) then begin
  panic('factors 1 failed');
end;
  if list_int_to_str(factors(100)) <> list_int_to_str([1, 2, 4, 5, 10, 20, 25, 50]) then begin
  panic('factors 100 failed');
end;
  if abundant(0) <> true then begin
  panic('abundant 0 failed');
end;
  if abundant(1) <> false then begin
  panic('abundant 1 failed');
end;
  if abundant(12) <> true then begin
  panic('abundant 12 failed');
end;
  if abundant(13) <> false then begin
  panic('abundant 13 failed');
end;
  if abundant(20) <> true then begin
  panic('abundant 20 failed');
end;
  if semi_perfect(0) <> true then begin
  panic('semi_perfect 0 failed');
end;
  if semi_perfect(1) <> true then begin
  panic('semi_perfect 1 failed');
end;
  if semi_perfect(12) <> true then begin
  panic('semi_perfect 12 failed');
end;
  if semi_perfect(13) <> false then begin
  panic('semi_perfect 13 failed');
end;
  if weird(0) <> false then begin
  panic('weird 0 failed');
end;
  if weird(70) <> true then begin
  panic('weird 70 failed');
end;
  if weird(77) <> false then begin
  panic('weird 77 failed');
end;
end;
procedure main();
var
  main_nums: array of integer;
  main_i: integer;
  main_n: integer;
begin
  run_tests();
  main_nums := [69, 70, 71];
  main_i := 0;
  while main_i < Length(main_nums) do begin
  main_n := main_nums[main_i];
  if weird(main_n) then begin
  writeln(IntToStr(main_n) + ' is weird.');
end else begin
  writeln(IntToStr(main_n) + ' is not weird.');
end;
  main_i := main_i + 1;
end;
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
