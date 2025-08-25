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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
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
function binary_search(binary_search_arr: IntArray; binary_search_lower_bound: int64; binary_search_upper_bound: int64; binary_search_value: int64): int64; forward;
function mat_bin_search(mat_bin_search_value: int64; mat_bin_search_matrix: IntArrayArray): IntArray; forward;
procedure main(); forward;
function binary_search(binary_search_arr: IntArray; binary_search_lower_bound: int64; binary_search_upper_bound: int64; binary_search_value: int64): int64;
var
  binary_search_r: int64;
begin
  binary_search_r := _floordiv(binary_search_lower_bound + binary_search_upper_bound, 2);
  if binary_search_arr[binary_search_r] = binary_search_value then begin
  exit(binary_search_r);
end;
  if binary_search_lower_bound >= binary_search_upper_bound then begin
  exit(-1);
end;
  if binary_search_arr[binary_search_r] < binary_search_value then begin
  exit(binary_search(binary_search_arr, binary_search_r + 1, binary_search_upper_bound, binary_search_value));
end;
  exit(binary_search(binary_search_arr, binary_search_lower_bound, binary_search_r - 1, binary_search_value));
end;
function mat_bin_search(mat_bin_search_value: int64; mat_bin_search_matrix: IntArrayArray): IntArray;
var
  mat_bin_search_index: int64;
  mat_bin_search_r: int64;
begin
  mat_bin_search_index := 0;
  if mat_bin_search_matrix[mat_bin_search_index][0] = mat_bin_search_value then begin
  exit([mat_bin_search_index, 0]);
end;
  while (mat_bin_search_index < Length(mat_bin_search_matrix)) and (mat_bin_search_matrix[mat_bin_search_index][0] < mat_bin_search_value) do begin
  mat_bin_search_r := binary_search(mat_bin_search_matrix[mat_bin_search_index], 0, Length(mat_bin_search_matrix[mat_bin_search_index]) - 1, mat_bin_search_value);
  if mat_bin_search_r <> -1 then begin
  exit([mat_bin_search_index, mat_bin_search_r]);
end;
  mat_bin_search_index := mat_bin_search_index + 1;
end;
  exit([-1, -1]);
end;
procedure main();
var
  main_row: array of int64;
  main_matrix: array of IntArray;
begin
  main_row := [1, 4, 7, 11, 15];
  writeln(IntToStr(binary_search(main_row, 0, Length(main_row) - 1, 1)));
  writeln(IntToStr(binary_search(main_row, 0, Length(main_row) - 1, 23)));
  main_matrix := [[1, 4, 7, 11, 15], [2, 5, 8, 12, 19], [3, 6, 9, 16, 22], [10, 13, 14, 17, 24], [18, 21, 23, 26, 30]];
  writeln(list_int_to_str(mat_bin_search(1, main_matrix)));
  writeln(list_int_to_str(mat_bin_search(34, main_matrix)));
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
