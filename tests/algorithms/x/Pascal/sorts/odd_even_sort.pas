{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function odd_even_sort(odd_even_sort_xs: IntArray): IntArray; forward;
procedure print_list(print_list_xs: IntArray); forward;
procedure test_odd_even_sort(); forward;
procedure main(); forward;
function odd_even_sort(odd_even_sort_xs: IntArray): IntArray;
var
  odd_even_sort_arr: array of int64;
  odd_even_sort_i: int64;
  odd_even_sort_n: integer;
  odd_even_sort_sorted: boolean;
  odd_even_sort_j: int64;
  odd_even_sort_tmp: int64;
  odd_even_sort_tmp_10: int64;
begin
  odd_even_sort_arr := [];
  odd_even_sort_i := 0;
  while odd_even_sort_i < Length(odd_even_sort_xs) do begin
  odd_even_sort_arr := concat(odd_even_sort_arr, IntArray([odd_even_sort_xs[odd_even_sort_i]]));
  odd_even_sort_i := odd_even_sort_i + 1;
end;
  odd_even_sort_n := Length(odd_even_sort_arr);
  odd_even_sort_sorted := false;
  while odd_even_sort_sorted = false do begin
  odd_even_sort_sorted := true;
  odd_even_sort_j := 0;
  while odd_even_sort_j < (odd_even_sort_n - 1) do begin
  if odd_even_sort_arr[odd_even_sort_j] > odd_even_sort_arr[odd_even_sort_j + 1] then begin
  odd_even_sort_tmp := odd_even_sort_arr[odd_even_sort_j];
  odd_even_sort_arr[odd_even_sort_j] := odd_even_sort_arr[odd_even_sort_j + 1];
  odd_even_sort_arr[odd_even_sort_j + 1] := odd_even_sort_tmp;
  odd_even_sort_sorted := false;
end;
  odd_even_sort_j := odd_even_sort_j + 2;
end;
  odd_even_sort_j := 1;
  while odd_even_sort_j < (odd_even_sort_n - 1) do begin
  if odd_even_sort_arr[odd_even_sort_j] > odd_even_sort_arr[odd_even_sort_j + 1] then begin
  odd_even_sort_tmp_10 := odd_even_sort_arr[odd_even_sort_j];
  odd_even_sort_arr[odd_even_sort_j] := odd_even_sort_arr[odd_even_sort_j + 1];
  odd_even_sort_arr[odd_even_sort_j + 1] := odd_even_sort_tmp_10;
  odd_even_sort_sorted := false;
end;
  odd_even_sort_j := odd_even_sort_j + 2;
end;
end;
  exit(odd_even_sort_arr);
end;
procedure print_list(print_list_xs: IntArray);
var
  print_list_i: int64;
  print_list_out_: string;
begin
  print_list_i := 0;
  print_list_out_ := '';
  while print_list_i < Length(print_list_xs) do begin
  if print_list_i > 0 then begin
  print_list_out_ := print_list_out_ + ' ';
end;
  print_list_out_ := print_list_out_ + IntToStr(print_list_xs[print_list_i]);
  print_list_i := print_list_i + 1;
end;
  writeln(print_list_out_);
end;
procedure test_odd_even_sort();
var
  test_odd_even_sort_a: array of int64;
  test_odd_even_sort_r1: IntArray;
  test_odd_even_sort_b: array of int64;
  test_odd_even_sort_r2: IntArray;
  test_odd_even_sort_c: array of int64;
  test_odd_even_sort_r3: IntArray;
  test_odd_even_sort_d: array of int64;
  test_odd_even_sort_r4: IntArray;
begin
  test_odd_even_sort_a := [5, 4, 3, 2, 1];
  test_odd_even_sort_r1 := odd_even_sort(test_odd_even_sort_a);
  if ((((test_odd_even_sort_r1[0] <> 1) or (test_odd_even_sort_r1[1] <> 2)) or (test_odd_even_sort_r1[2] <> 3)) or (test_odd_even_sort_r1[3] <> 4)) or (test_odd_even_sort_r1[4] <> 5) then begin
  panic('case1 failed');
end;
  test_odd_even_sort_b := [];
  test_odd_even_sort_r2 := odd_even_sort(test_odd_even_sort_b);
  if Length(test_odd_even_sort_r2) <> 0 then begin
  panic('case2 failed');
end;
  test_odd_even_sort_c := [-10, -1, 10, 2];
  test_odd_even_sort_r3 := odd_even_sort(test_odd_even_sort_c);
  if (((test_odd_even_sort_r3[0] <> -10) or (test_odd_even_sort_r3[1] <> -1)) or (test_odd_even_sort_r3[2] <> 2)) or (test_odd_even_sort_r3[3] <> 10) then begin
  panic('case3 failed');
end;
  test_odd_even_sort_d := [1, 2, 3, 4];
  test_odd_even_sort_r4 := odd_even_sort(test_odd_even_sort_d);
  if (((test_odd_even_sort_r4[0] <> 1) or (test_odd_even_sort_r4[1] <> 2)) or (test_odd_even_sort_r4[2] <> 3)) or (test_odd_even_sort_r4[3] <> 4) then begin
  panic('case4 failed');
end;
end;
procedure main();
var
  main_sample: array of int64;
  main_sorted: IntArray;
begin
  test_odd_even_sort();
  main_sample := [5, 4, 3, 2, 1];
  main_sorted := odd_even_sort(main_sample);
  print_list(main_sorted);
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
  writeln('');
end.
