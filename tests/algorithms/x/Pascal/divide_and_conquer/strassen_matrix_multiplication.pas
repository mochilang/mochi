{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type IntArrayArrayArray = array of IntArrayArray;
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
function max(xs: array of integer): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin max := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];
  max := m;
end;
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  matrix_a: IntArrayArray;
  n: integer;
  rows: integer;
  a: IntArrayArray;
  b: IntArrayArray;
  mat: IntArrayArray;
  matrix2: IntArrayArray;
  cols: integer;
  matrix_b: IntArrayArray;
  matrix: IntArrayArray;
  matrix1: IntArrayArray;
function default_matrix_multiplication(a: IntArrayArray; b: IntArrayArray): IntArrayArray; forward;
function matrix_addition(matrix_a: IntArrayArray; matrix_b: IntArrayArray): IntArrayArray; forward;
function matrix_subtraction(matrix_a: IntArrayArray; matrix_b: IntArrayArray): IntArrayArray; forward;
function split_matrix(a: IntArrayArray): IntArrayArrayArray; forward;
function matrix_dimensions(matrix: IntArrayArray): IntArray; forward;
function next_power_of_two(n: integer): integer; forward;
function pad_matrix(mat: IntArrayArray; rows: integer; cols: integer): IntArrayArray; forward;
function actual_strassen(matrix_a: IntArrayArray; matrix_b: IntArrayArray): IntArrayArray; forward;
function strassen(matrix1: IntArrayArray; matrix2: IntArrayArray): IntArrayArray; forward;
procedure main(); forward;
function default_matrix_multiplication(a: IntArrayArray; b: IntArrayArray): IntArrayArray;
begin
  exit([[(a[0][0] * b[0][0]) + (a[0][1] * b[1][0]), (a[0][0] * b[0][1]) + (a[0][1] * b[1][1])], [(a[1][0] * b[0][0]) + (a[1][1] * b[1][0]), (a[1][0] * b[0][1]) + (a[1][1] * b[1][1])]]);
end;
function matrix_addition(matrix_a: IntArrayArray; matrix_b: IntArrayArray): IntArrayArray;
var
  matrix_addition_result_: array of IntArray;
  matrix_addition_i: integer;
  matrix_addition_row: array of integer;
  matrix_addition_j: integer;
begin
  matrix_addition_result_ := [];
  matrix_addition_i := 0;
  while matrix_addition_i < Length(matrix_a) do begin
  matrix_addition_row := [];
  matrix_addition_j := 0;
  while matrix_addition_j < Length(matrix_a[matrix_addition_i]) do begin
  matrix_addition_row := concat(matrix_addition_row, IntArray([matrix_a[matrix_addition_i][matrix_addition_j] + matrix_b[matrix_addition_i][matrix_addition_j]]));
  matrix_addition_j := matrix_addition_j + 1;
end;
  matrix_addition_result_ := concat(matrix_addition_result_, [matrix_addition_row]);
  matrix_addition_i := matrix_addition_i + 1;
end;
  exit(matrix_addition_result_);
end;
function matrix_subtraction(matrix_a: IntArrayArray; matrix_b: IntArrayArray): IntArrayArray;
var
  matrix_subtraction_result_: array of IntArray;
  matrix_subtraction_i: integer;
  matrix_subtraction_row: array of integer;
  matrix_subtraction_j: integer;
begin
  matrix_subtraction_result_ := [];
  matrix_subtraction_i := 0;
  while matrix_subtraction_i < Length(matrix_a) do begin
  matrix_subtraction_row := [];
  matrix_subtraction_j := 0;
  while matrix_subtraction_j < Length(matrix_a[matrix_subtraction_i]) do begin
  matrix_subtraction_row := concat(matrix_subtraction_row, IntArray([matrix_a[matrix_subtraction_i][matrix_subtraction_j] - matrix_b[matrix_subtraction_i][matrix_subtraction_j]]));
  matrix_subtraction_j := matrix_subtraction_j + 1;
end;
  matrix_subtraction_result_ := concat(matrix_subtraction_result_, [matrix_subtraction_row]);
  matrix_subtraction_i := matrix_subtraction_i + 1;
end;
  exit(matrix_subtraction_result_);
end;
function split_matrix(a: IntArrayArray): IntArrayArrayArray;
var
  split_matrix_n: integer;
  split_matrix_mid: integer;
  split_matrix_top_left: array of IntArray;
  split_matrix_top_right: array of IntArray;
  split_matrix_bot_left: array of IntArray;
  split_matrix_bot_right: array of IntArray;
  split_matrix_i: integer;
  split_matrix_left_row: array of integer;
  split_matrix_right_row: array of integer;
  split_matrix_j: integer;
begin
  split_matrix_n := Length(a);
  split_matrix_mid := split_matrix_n div 2;
  split_matrix_top_left := [];
  split_matrix_top_right := [];
  split_matrix_bot_left := [];
  split_matrix_bot_right := [];
  split_matrix_i := 0;
  while split_matrix_i < split_matrix_mid do begin
  split_matrix_left_row := [];
  split_matrix_right_row := [];
  split_matrix_j := 0;
  while split_matrix_j < split_matrix_mid do begin
  split_matrix_left_row := concat(split_matrix_left_row, IntArray([a[split_matrix_i][split_matrix_j]]));
  split_matrix_right_row := concat(split_matrix_right_row, IntArray([a[split_matrix_i][split_matrix_j + split_matrix_mid]]));
  split_matrix_j := split_matrix_j + 1;
end;
  split_matrix_top_left := concat(split_matrix_top_left, [split_matrix_left_row]);
  split_matrix_top_right := concat(split_matrix_top_right, [split_matrix_right_row]);
  split_matrix_i := split_matrix_i + 1;
end;
  split_matrix_i := split_matrix_mid;
  while split_matrix_i < split_matrix_n do begin
  split_matrix_left_row := [];
  split_matrix_right_row := [];
  split_matrix_j := 0;
  while split_matrix_j < split_matrix_mid do begin
  split_matrix_left_row := concat(split_matrix_left_row, IntArray([a[split_matrix_i][split_matrix_j]]));
  split_matrix_right_row := concat(split_matrix_right_row, IntArray([a[split_matrix_i][split_matrix_j + split_matrix_mid]]));
  split_matrix_j := split_matrix_j + 1;
end;
  split_matrix_bot_left := concat(split_matrix_bot_left, [split_matrix_left_row]);
  split_matrix_bot_right := concat(split_matrix_bot_right, [split_matrix_right_row]);
  split_matrix_i := split_matrix_i + 1;
end;
  exit([split_matrix_top_left, split_matrix_top_right, split_matrix_bot_left, split_matrix_bot_right]);
end;
function matrix_dimensions(matrix: IntArrayArray): IntArray;
begin
  exit([Length(matrix), Length(matrix[0])]);
end;
function next_power_of_two(n: integer): integer;
var
  next_power_of_two_p: integer;
begin
  next_power_of_two_p := 1;
  while next_power_of_two_p < n do begin
  next_power_of_two_p := next_power_of_two_p * 2;
end;
  exit(next_power_of_two_p);
end;
function pad_matrix(mat: IntArrayArray; rows: integer; cols: integer): IntArrayArray;
var
  pad_matrix_res: array of IntArray;
  pad_matrix_i: integer;
  pad_matrix_row: array of integer;
  pad_matrix_j: integer;
  pad_matrix_v: integer;
begin
  pad_matrix_res := [];
  pad_matrix_i := 0;
  while pad_matrix_i < rows do begin
  pad_matrix_row := [];
  pad_matrix_j := 0;
  while pad_matrix_j < cols do begin
  pad_matrix_v := 0;
  if (pad_matrix_i < Length(mat)) and (pad_matrix_j < Length(mat[0])) then begin
  pad_matrix_v := mat[pad_matrix_i][pad_matrix_j];
end;
  pad_matrix_row := concat(pad_matrix_row, IntArray([pad_matrix_v]));
  pad_matrix_j := pad_matrix_j + 1;
end;
  pad_matrix_res := concat(pad_matrix_res, [pad_matrix_row]);
  pad_matrix_i := pad_matrix_i + 1;
end;
  exit(pad_matrix_res);
end;
function actual_strassen(matrix_a: IntArrayArray; matrix_b: IntArrayArray): IntArrayArray;
var
  actual_strassen_parts_a: IntArrayArrayArray;
  actual_strassen_a: array of IntArray;
  actual_strassen_b: array of IntArray;
  actual_strassen_c: array of IntArray;
  actual_strassen_d: array of IntArray;
  actual_strassen_parts_b: IntArrayArrayArray;
  actual_strassen_e: array of IntArray;
  actual_strassen_f: array of IntArray;
  actual_strassen_g: array of IntArray;
  actual_strassen_h: array of IntArray;
  actual_strassen_t1: array of IntArray;
  actual_strassen_t2: array of IntArray;
  actual_strassen_t3: array of IntArray;
  actual_strassen_t4: array of IntArray;
  actual_strassen_t5: array of IntArray;
  actual_strassen_t6: array of IntArray;
  actual_strassen_t7: array of IntArray;
  actual_strassen_top_left: IntArrayArray;
  actual_strassen_top_right: IntArrayArray;
  actual_strassen_bot_left: IntArrayArray;
  actual_strassen_bot_right: IntArrayArray;
  actual_strassen_new_matrix: array of IntArray;
  actual_strassen_i: integer;
begin
  if matrix_dimensions(matrix_a)[0] = 2 then begin
  exit(default_matrix_multiplication(matrix_a, matrix_b));
end;
  actual_strassen_parts_a := split_matrix(matrix_a);
  actual_strassen_a := actual_strassen_parts_a[0];
  actual_strassen_b := actual_strassen_parts_a[1];
  actual_strassen_c := actual_strassen_parts_a[2];
  actual_strassen_d := actual_strassen_parts_a[3];
  actual_strassen_parts_b := split_matrix(matrix_b);
  actual_strassen_e := actual_strassen_parts_b[0];
  actual_strassen_f := actual_strassen_parts_b[1];
  actual_strassen_g := actual_strassen_parts_b[2];
  actual_strassen_h := actual_strassen_parts_b[3];
  actual_strassen_t1 := actual_strassen(actual_strassen_a, matrix_subtraction(actual_strassen_f, actual_strassen_h));
  actual_strassen_t2 := actual_strassen(matrix_addition(actual_strassen_a, actual_strassen_b), actual_strassen_h);
  actual_strassen_t3 := actual_strassen(matrix_addition(actual_strassen_c, actual_strassen_d), actual_strassen_e);
  actual_strassen_t4 := actual_strassen(actual_strassen_d, matrix_subtraction(actual_strassen_g, actual_strassen_e));
  actual_strassen_t5 := actual_strassen(matrix_addition(actual_strassen_a, actual_strassen_d), matrix_addition(actual_strassen_e, actual_strassen_h));
  actual_strassen_t6 := actual_strassen(matrix_subtraction(actual_strassen_b, actual_strassen_d), matrix_addition(actual_strassen_g, actual_strassen_h));
  actual_strassen_t7 := actual_strassen(matrix_subtraction(actual_strassen_a, actual_strassen_c), matrix_addition(actual_strassen_e, actual_strassen_f));
  actual_strassen_top_left := matrix_addition(matrix_subtraction(matrix_addition(actual_strassen_t5, actual_strassen_t4), actual_strassen_t2), actual_strassen_t6);
  actual_strassen_top_right := matrix_addition(actual_strassen_t1, actual_strassen_t2);
  actual_strassen_bot_left := matrix_addition(actual_strassen_t3, actual_strassen_t4);
  actual_strassen_bot_right := matrix_subtraction(matrix_subtraction(matrix_addition(actual_strassen_t1, actual_strassen_t5), actual_strassen_t3), actual_strassen_t7);
  actual_strassen_new_matrix := [];
  actual_strassen_i := 0;
  while actual_strassen_i < Length(actual_strassen_top_right) do begin
  actual_strassen_new_matrix := concat(actual_strassen_new_matrix, [concat(actual_strassen_top_left[actual_strassen_i], actual_strassen_top_right[actual_strassen_i])]);
  actual_strassen_i := actual_strassen_i + 1;
end;
  actual_strassen_i := 0;
  while actual_strassen_i < Length(actual_strassen_bot_right) do begin
  actual_strassen_new_matrix := concat(actual_strassen_new_matrix, [concat(actual_strassen_bot_left[actual_strassen_i], actual_strassen_bot_right[actual_strassen_i])]);
  actual_strassen_i := actual_strassen_i + 1;
end;
  exit(actual_strassen_new_matrix);
end;
function strassen(matrix1: IntArrayArray; matrix2: IntArrayArray): IntArrayArray;
var
  strassen_dims1: IntArray;
  strassen_dims2: IntArray;
  strassen_maximum: integer;
  strassen_size: integer;
  strassen_new_matrix1: IntArrayArray;
  strassen_new_matrix2: IntArrayArray;
  strassen_result_padded: IntArrayArray;
  strassen_final_matrix: array of IntArray;
  strassen_i: integer;
  strassen_row: array of integer;
  strassen_j: integer;
begin
  strassen_dims1 := matrix_dimensions(matrix1);
  strassen_dims2 := matrix_dimensions(matrix2);
  if strassen_dims1[1] <> strassen_dims2[0] then begin
  exit([]);
end;
  strassen_maximum := Trunc(max([strassen_dims1[0], strassen_dims1[1], strassen_dims2[0], strassen_dims2[1]]));
  strassen_size := next_power_of_two(strassen_maximum);
  strassen_new_matrix1 := pad_matrix(matrix1, strassen_size, strassen_size);
  strassen_new_matrix2 := pad_matrix(matrix2, strassen_size, strassen_size);
  strassen_result_padded := actual_strassen(strassen_new_matrix1, strassen_new_matrix2);
  strassen_final_matrix := [];
  strassen_i := 0;
  while strassen_i < strassen_dims1[0] do begin
  strassen_row := [];
  strassen_j := 0;
  while strassen_j < strassen_dims2[1] do begin
  strassen_row := concat(strassen_row, IntArray([strassen_result_padded[strassen_i][strassen_j]]));
  strassen_j := strassen_j + 1;
end;
  strassen_final_matrix := concat(strassen_final_matrix, [strassen_row]);
  strassen_i := strassen_i + 1;
end;
  exit(strassen_final_matrix);
end;
procedure main();
var
  main_matrix1: array of IntArray;
  main_matrix2: array of IntArray;
  main_res: IntArrayArray;
begin
  main_matrix1 := [[2, 3, 4, 5], [6, 4, 3, 1], [2, 3, 6, 7], [3, 1, 2, 4], [2, 3, 4, 5], [6, 4, 3, 1], [2, 3, 6, 7], [3, 1, 2, 4], [2, 3, 4, 5], [6, 2, 3, 1]];
  main_matrix2 := [[0, 2, 1, 1], [16, 2, 3, 3], [2, 2, 7, 7], [13, 11, 22, 4]];
  main_res := strassen(main_matrix1, main_matrix2);
  show_list_list(main_res);
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
