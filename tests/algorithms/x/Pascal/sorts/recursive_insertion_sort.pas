{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure json(x: int64);
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
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
function insert_next(insert_next_collection: IntArray; insert_next_index: int64): IntArray; forward;
function rec_insertion_sort(rec_insertion_sort_collection: IntArray; rec_insertion_sort_n: int64): IntArray; forward;
procedure test_rec_insertion_sort(); forward;
procedure main(); forward;
function insert_next(insert_next_collection: IntArray; insert_next_index: int64): IntArray;
var
  insert_next_arr: array of int64;
  insert_next_j: int64;
  insert_next_temp: int64;
begin
  insert_next_arr := insert_next_collection;
  if (insert_next_index >= Length(insert_next_arr)) or (insert_next_arr[insert_next_index - 1] <= insert_next_arr[insert_next_index]) then begin
  exit(insert_next_arr);
end;
  insert_next_j := insert_next_index - 1;
  insert_next_temp := insert_next_arr[insert_next_j];
  insert_next_arr[insert_next_j] := insert_next_arr[insert_next_index];
  insert_next_arr[insert_next_index] := insert_next_temp;
  exit(insert_next(insert_next_arr, insert_next_index + 1));
end;
function rec_insertion_sort(rec_insertion_sort_collection: IntArray; rec_insertion_sort_n: int64): IntArray;
var
  rec_insertion_sort_arr: array of int64;
begin
  rec_insertion_sort_arr := rec_insertion_sort_collection;
  if (Length(rec_insertion_sort_arr) <= 1) or (rec_insertion_sort_n <= 1) then begin
  exit(rec_insertion_sort_arr);
end;
  rec_insertion_sort_arr := insert_next(rec_insertion_sort_arr, rec_insertion_sort_n - 1);
  exit(rec_insertion_sort(rec_insertion_sort_arr, rec_insertion_sort_n - 1));
end;
procedure test_rec_insertion_sort();
var
  test_rec_insertion_sort_col1: array of int64;
  test_rec_insertion_sort_col2: array of int64;
  test_rec_insertion_sort_col3: array of int64;
begin
  test_rec_insertion_sort_col1 := [1, 2, 1];
  test_rec_insertion_sort_col1 := rec_insertion_sort(test_rec_insertion_sort_col1, Length(test_rec_insertion_sort_col1));
  if ((test_rec_insertion_sort_col1[0] <> 1) or (test_rec_insertion_sort_col1[1] <> 1)) or (test_rec_insertion_sort_col1[2] <> 2) then begin
  panic('test1 failed');
end;
  test_rec_insertion_sort_col2 := [2, 1, 0, -1, -2];
  test_rec_insertion_sort_col2 := rec_insertion_sort(test_rec_insertion_sort_col2, Length(test_rec_insertion_sort_col2));
  if test_rec_insertion_sort_col2[0] <> (0 - 2) then begin
  panic('test2 failed');
end;
  if test_rec_insertion_sort_col2[1] <> (0 - 1) then begin
  panic('test2 failed');
end;
  if test_rec_insertion_sort_col2[2] <> 0 then begin
  panic('test2 failed');
end;
  if test_rec_insertion_sort_col2[3] <> 1 then begin
  panic('test2 failed');
end;
  if test_rec_insertion_sort_col2[4] <> 2 then begin
  panic('test2 failed');
end;
  test_rec_insertion_sort_col3 := [1];
  test_rec_insertion_sort_col3 := rec_insertion_sort(test_rec_insertion_sort_col3, Length(test_rec_insertion_sort_col3));
  if test_rec_insertion_sort_col3[0] <> 1 then begin
  panic('test3 failed');
end;
end;
procedure main();
var
  main_numbers: array of int64;
begin
  test_rec_insertion_sort();
  main_numbers := [5, 3, 4, 1, 2];
  main_numbers := rec_insertion_sort(main_numbers, Length(main_numbers));
  writeln(list_int_to_str(main_numbers));
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
