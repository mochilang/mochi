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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  mat: IntArrayArray;
  r90: IntArrayArray;
  r180: IntArrayArray;
  r270: IntArrayArray;
  row_size: integer;
  n: integer;
  row: IntArray;
function abs_int(n: integer): integer; forward;
function make_matrix(row_size: integer): IntArrayArray; forward;
function transpose(mat: IntArrayArray): IntArrayArray; forward;
function reverse_row(mat: IntArrayArray): IntArrayArray; forward;
function reverse_column(mat: IntArrayArray): IntArrayArray; forward;
function rotate_90(mat: IntArrayArray): IntArrayArray; forward;
function rotate_180(mat: IntArrayArray): IntArrayArray; forward;
function rotate_270(mat: IntArrayArray): IntArrayArray; forward;
function row_to_string(row: IntArray): string; forward;
procedure print_matrix(mat: IntArrayArray); forward;
function abs_int(n: integer): integer;
begin
  if n < 0 then begin
  exit(-n);
end;
  exit(n);
end;
function make_matrix(row_size: integer): IntArrayArray;
var
  make_matrix_size: integer;
  make_matrix_mat: array of IntArray;
  make_matrix_y: integer;
  make_matrix_row: array of integer;
  make_matrix_x: integer;
begin
  make_matrix_size := abs_int(row_size);
  if make_matrix_size = 0 then begin
  make_matrix_size := 4;
end;
  make_matrix_mat := [];
  make_matrix_y := 0;
  while make_matrix_y < make_matrix_size do begin
  make_matrix_row := [];
  make_matrix_x := 0;
  while make_matrix_x < make_matrix_size do begin
  make_matrix_row := concat(make_matrix_row, IntArray([(1 + make_matrix_x) + (make_matrix_y * make_matrix_size)]));
  make_matrix_x := make_matrix_x + 1;
end;
  make_matrix_mat := concat(make_matrix_mat, [make_matrix_row]);
  make_matrix_y := make_matrix_y + 1;
end;
  exit(make_matrix_mat);
end;
function transpose(mat: IntArrayArray): IntArrayArray;
var
  transpose_n: integer;
  transpose_result_: array of IntArray;
  transpose_i: integer;
  transpose_row: array of integer;
  transpose_j: integer;
begin
  transpose_n := Length(mat);
  transpose_result_ := [];
  transpose_i := 0;
  while transpose_i < transpose_n do begin
  transpose_row := [];
  transpose_j := 0;
  while transpose_j < transpose_n do begin
  transpose_row := concat(transpose_row, IntArray([mat[transpose_j][transpose_i]]));
  transpose_j := transpose_j + 1;
end;
  transpose_result_ := concat(transpose_result_, [transpose_row]);
  transpose_i := transpose_i + 1;
end;
  exit(transpose_result_);
end;
function reverse_row(mat: IntArrayArray): IntArrayArray;
var
  reverse_row_result_: array of IntArray;
  reverse_row_i: integer;
begin
  reverse_row_result_ := [];
  reverse_row_i := Length(mat) - 1;
  while reverse_row_i >= 0 do begin
  reverse_row_result_ := concat(reverse_row_result_, [mat[reverse_row_i]]);
  reverse_row_i := reverse_row_i - 1;
end;
  exit(reverse_row_result_);
end;
function reverse_column(mat: IntArrayArray): IntArrayArray;
var
  reverse_column_result_: array of IntArray;
  reverse_column_i: integer;
  reverse_column_row: array of integer;
  reverse_column_j: integer;
begin
  reverse_column_result_ := [];
  reverse_column_i := 0;
  while reverse_column_i < Length(mat) do begin
  reverse_column_row := [];
  reverse_column_j := Length(mat[reverse_column_i]) - 1;
  while reverse_column_j >= 0 do begin
  reverse_column_row := concat(reverse_column_row, IntArray([mat[reverse_column_i][reverse_column_j]]));
  reverse_column_j := reverse_column_j - 1;
end;
  reverse_column_result_ := concat(reverse_column_result_, [reverse_column_row]);
  reverse_column_i := reverse_column_i + 1;
end;
  exit(reverse_column_result_);
end;
function rotate_90(mat: IntArrayArray): IntArrayArray;
var
  rotate_90_t: array of IntArray;
  rotate_90_rr: array of IntArray;
begin
  rotate_90_t := transpose(mat);
  rotate_90_rr := reverse_row(rotate_90_t);
  exit(rotate_90_rr);
end;
function rotate_180(mat: IntArrayArray): IntArrayArray;
var
  rotate_180_rc: array of IntArray;
  rotate_180_rr: array of IntArray;
begin
  rotate_180_rc := reverse_column(mat);
  rotate_180_rr := reverse_row(rotate_180_rc);
  exit(rotate_180_rr);
end;
function rotate_270(mat: IntArrayArray): IntArrayArray;
var
  rotate_270_t: array of IntArray;
  rotate_270_rc: array of IntArray;
begin
  rotate_270_t := transpose(mat);
  rotate_270_rc := reverse_column(rotate_270_t);
  exit(rotate_270_rc);
end;
function row_to_string(row: IntArray): string;
var
  row_to_string_line: string;
  row_to_string_i: integer;
begin
  row_to_string_line := '';
  row_to_string_i := 0;
  while row_to_string_i < Length(row) do begin
  if row_to_string_i = 0 then begin
  row_to_string_line := IntToStr(row[row_to_string_i]);
end else begin
  row_to_string_line := (row_to_string_line + ' ') + IntToStr(row[row_to_string_i]);
end;
  row_to_string_i := row_to_string_i + 1;
end;
  exit(row_to_string_line);
end;
procedure print_matrix(mat: IntArrayArray);
var
  print_matrix_i: integer;
begin
  print_matrix_i := 0;
  while print_matrix_i < Length(mat) do begin
  writeln(row_to_string(mat[print_matrix_i]));
  print_matrix_i := print_matrix_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  mat := make_matrix(4);
  writeln('' + #10 + 'origin:' + #10 + '');
  print_matrix(mat);
  writeln('' + #10 + 'rotate 90 counterclockwise:' + #10 + '');
  r90 := rotate_90(mat);
  print_matrix(r90);
  mat := make_matrix(4);
  writeln('' + #10 + 'origin:' + #10 + '');
  print_matrix(mat);
  writeln('' + #10 + 'rotate 180:' + #10 + '');
  r180 := rotate_180(mat);
  print_matrix(r180);
  mat := make_matrix(4);
  writeln('' + #10 + 'origin:' + #10 + '');
  print_matrix(mat);
  writeln('' + #10 + 'rotate 270 counterclockwise:' + #10 + '');
  r270 := rotate_270(mat);
  print_matrix(r270);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
