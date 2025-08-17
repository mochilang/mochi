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
function is_valid_matrix(is_valid_matrix_matrix: IntArrayArray): boolean; forward;
function spiral_traversal(spiral_traversal_matrix: IntArrayArray): IntArray; forward;
procedure spiral_print_clockwise(spiral_print_clockwise_matrix: IntArrayArray); forward;
procedure main(); forward;
function is_valid_matrix(is_valid_matrix_matrix: IntArrayArray): boolean;
var
  is_valid_matrix_cols: integer;
  is_valid_matrix_row: IntArray;
begin
  if Length(is_valid_matrix_matrix) = 0 then begin
  exit(false);
end;
  is_valid_matrix_cols := Length(is_valid_matrix_matrix[0]);
  for is_valid_matrix_row in is_valid_matrix_matrix do begin
  if Length(is_valid_matrix_row) <> is_valid_matrix_cols then begin
  exit(false);
end;
end;
  exit(true);
end;
function spiral_traversal(spiral_traversal_matrix: IntArrayArray): IntArray;
var
  spiral_traversal_rows: integer;
  spiral_traversal_cols: integer;
  spiral_traversal_top: integer;
  spiral_traversal_bottom: integer;
  spiral_traversal_left: integer;
  spiral_traversal_right: integer;
  spiral_traversal_result_: array of integer;
  spiral_traversal_i: integer;
begin
  if not is_valid_matrix(spiral_traversal_matrix) then begin
  exit([]);
end;
  spiral_traversal_rows := Length(spiral_traversal_matrix);
  spiral_traversal_cols := Length(spiral_traversal_matrix[0]);
  spiral_traversal_top := 0;
  spiral_traversal_bottom := spiral_traversal_rows - 1;
  spiral_traversal_left := 0;
  spiral_traversal_right := spiral_traversal_cols - 1;
  spiral_traversal_result_ := [];
  while (spiral_traversal_left <= spiral_traversal_right) and (spiral_traversal_top <= spiral_traversal_bottom) do begin
  spiral_traversal_i := spiral_traversal_left;
  while spiral_traversal_i <= spiral_traversal_right do begin
  spiral_traversal_result_ := concat(spiral_traversal_result_, IntArray([spiral_traversal_matrix[spiral_traversal_top][spiral_traversal_i]]));
  spiral_traversal_i := spiral_traversal_i + 1;
end;
  spiral_traversal_top := spiral_traversal_top + 1;
  spiral_traversal_i := spiral_traversal_top;
  while spiral_traversal_i <= spiral_traversal_bottom do begin
  spiral_traversal_result_ := concat(spiral_traversal_result_, IntArray([spiral_traversal_matrix[spiral_traversal_i][spiral_traversal_right]]));
  spiral_traversal_i := spiral_traversal_i + 1;
end;
  spiral_traversal_right := spiral_traversal_right - 1;
  if spiral_traversal_top <= spiral_traversal_bottom then begin
  spiral_traversal_i := spiral_traversal_right;
  while spiral_traversal_i >= spiral_traversal_left do begin
  spiral_traversal_result_ := concat(spiral_traversal_result_, IntArray([spiral_traversal_matrix[spiral_traversal_bottom][spiral_traversal_i]]));
  spiral_traversal_i := spiral_traversal_i - 1;
end;
  spiral_traversal_bottom := spiral_traversal_bottom - 1;
end;
  if spiral_traversal_left <= spiral_traversal_right then begin
  spiral_traversal_i := spiral_traversal_bottom;
  while spiral_traversal_i >= spiral_traversal_top do begin
  spiral_traversal_result_ := concat(spiral_traversal_result_, IntArray([spiral_traversal_matrix[spiral_traversal_i][spiral_traversal_left]]));
  spiral_traversal_i := spiral_traversal_i - 1;
end;
  spiral_traversal_left := spiral_traversal_left + 1;
end;
end;
  exit(spiral_traversal_result_);
end;
procedure spiral_print_clockwise(spiral_print_clockwise_matrix: IntArrayArray);
var
  spiral_print_clockwise_value: integer;
begin
  for spiral_print_clockwise_value in spiral_traversal(spiral_print_clockwise_matrix) do begin
  writeln(IntToStr(spiral_print_clockwise_value));
end;
end;
procedure main();
var
  main_a: array of array of integer;
begin
  main_a := [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]];
  spiral_print_clockwise(main_a);
  writeln(list_int_to_str(spiral_traversal(main_a)));
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
