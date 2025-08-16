{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type RealArrayArrayArray = array of RealArrayArray;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_real_to_str(xs: array of RealArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_real_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: RealArrayArray;
  row: integer;
  b: RealArrayArray;
  n: integer;
  matrices: RealArrayArrayArray;
  matrix: RealArrayArray;
  column: integer;
function add(matrices: RealArrayArrayArray): RealArrayArray; forward;
function subtract(a: RealArrayArray; b: RealArrayArray): RealArrayArray; forward;
function scalar_multiply(matrix: RealArrayArray; n: real): RealArrayArray; forward;
function multiply(a: RealArrayArray; b: RealArrayArray): RealArrayArray; forward;
function identity(n: integer): RealArrayArray; forward;
function transpose(matrix: RealArrayArray): RealArrayArray; forward;
function minor(matrix: RealArrayArray; row: integer; column: integer): RealArrayArray; forward;
function determinant(matrix: RealArrayArray): real; forward;
function inverse(matrix: RealArrayArray): RealArrayArray; forward;
procedure main(); forward;
function add(matrices: RealArrayArrayArray): RealArrayArray;
var
  add_rows: integer;
  add_cols: integer;
  add_r: integer;
  add_result_: array of RealArray;
  add_row: array of real;
  add_c: integer;
  add_sum: real;
  add_m: integer;
begin
  add_rows := Length(matrices[0]);
  add_cols := Length(matrices[0][0]);
  add_r := 0;
  add_result_ := [];
  while add_r < add_rows do begin
  add_row := [];
  add_c := 0;
  while add_c < add_cols do begin
  add_sum := 0;
  add_m := 0;
  while add_m < Length(matrices) do begin
  add_sum := add_sum + matrices[add_m][add_r][add_c];
  add_m := add_m + 1;
end;
  add_row := concat(add_row, [add_sum]);
  add_c := add_c + 1;
end;
  add_result_ := concat(add_result_, [add_row]);
  add_r := add_r + 1;
end;
  exit(add_result_);
end;
function subtract(a: RealArrayArray; b: RealArrayArray): RealArrayArray;
var
  subtract_rows: integer;
  subtract_cols: integer;
  subtract_r: integer;
  subtract_result_: array of RealArray;
  subtract_row: array of real;
  subtract_c: integer;
begin
  subtract_rows := Length(a);
  subtract_cols := Length(a[0]);
  subtract_r := 0;
  subtract_result_ := [];
  while subtract_r < subtract_rows do begin
  subtract_row := [];
  subtract_c := 0;
  while subtract_c < subtract_cols do begin
  subtract_row := concat(subtract_row, [a[subtract_r][subtract_c] - b[subtract_r][subtract_c]]);
  subtract_c := subtract_c + 1;
end;
  subtract_result_ := concat(subtract_result_, [subtract_row]);
  subtract_r := subtract_r + 1;
end;
  exit(subtract_result_);
end;
function scalar_multiply(matrix: RealArrayArray; n: real): RealArrayArray;
var
  scalar_multiply_result_: array of RealArray;
  scalar_multiply_i: integer;
  scalar_multiply_row: array of real;
  scalar_multiply_j: integer;
begin
  scalar_multiply_result_ := [];
  scalar_multiply_i := 0;
  while scalar_multiply_i < Length(matrix) do begin
  scalar_multiply_row := [];
  scalar_multiply_j := 0;
  while scalar_multiply_j < Length(matrix[scalar_multiply_i]) do begin
  scalar_multiply_row := concat(scalar_multiply_row, [matrix[scalar_multiply_i][scalar_multiply_j] * n]);
  scalar_multiply_j := scalar_multiply_j + 1;
end;
  scalar_multiply_result_ := concat(scalar_multiply_result_, [scalar_multiply_row]);
  scalar_multiply_i := scalar_multiply_i + 1;
end;
  exit(scalar_multiply_result_);
end;
function multiply(a: RealArrayArray; b: RealArrayArray): RealArrayArray;
var
  multiply_rowsA: integer;
  multiply_colsA: integer;
  multiply_rowsB: integer;
  multiply_colsB: integer;
  multiply_result_: array of RealArray;
  multiply_i: integer;
  multiply_row: array of real;
  multiply_j: integer;
  multiply_sum: real;
  multiply_k: integer;
begin
  multiply_rowsA := Length(a);
  multiply_colsA := Length(a[0]);
  multiply_rowsB := Length(b);
  multiply_colsB := Length(b[0]);
  multiply_result_ := [];
  multiply_i := 0;
  while multiply_i < multiply_rowsA do begin
  multiply_row := [];
  multiply_j := 0;
  while multiply_j < multiply_colsB do begin
  multiply_sum := 0;
  multiply_k := 0;
  while multiply_k < multiply_colsA do begin
  multiply_sum := multiply_sum + (a[multiply_i][multiply_k] * b[multiply_k][multiply_j]);
  multiply_k := multiply_k + 1;
end;
  multiply_row := concat(multiply_row, [multiply_sum]);
  multiply_j := multiply_j + 1;
end;
  multiply_result_ := concat(multiply_result_, [multiply_row]);
  multiply_i := multiply_i + 1;
end;
  exit(multiply_result_);
end;
function identity(n: integer): RealArrayArray;
var
  identity_result_: array of RealArray;
  identity_i: integer;
  identity_row: array of real;
  identity_j: integer;
begin
  identity_result_ := [];
  identity_i := 0;
  while identity_i < n do begin
  identity_row := [];
  identity_j := 0;
  while identity_j < n do begin
  if identity_i = identity_j then begin
  identity_row := concat(identity_row, [1]);
end else begin
  identity_row := concat(identity_row, [0]);
end;
  identity_j := identity_j + 1;
end;
  identity_result_ := concat(identity_result_, [identity_row]);
  identity_i := identity_i + 1;
end;
  exit(identity_result_);
end;
function transpose(matrix: RealArrayArray): RealArrayArray;
var
  transpose_rows: integer;
  transpose_cols: integer;
  transpose_result_: array of RealArray;
  transpose_c: integer;
  transpose_row: array of real;
  transpose_r: integer;
begin
  transpose_rows := Length(matrix);
  transpose_cols := Length(matrix[0]);
  transpose_result_ := [];
  transpose_c := 0;
  while transpose_c < transpose_cols do begin
  transpose_row := [];
  transpose_r := 0;
  while transpose_r < transpose_rows do begin
  transpose_row := concat(transpose_row, [matrix[transpose_r][transpose_c]]);
  transpose_r := transpose_r + 1;
end;
  transpose_result_ := concat(transpose_result_, [transpose_row]);
  transpose_c := transpose_c + 1;
end;
  exit(transpose_result_);
end;
function minor(matrix: RealArrayArray; row: integer; column: integer): RealArrayArray;
var
  minor_result_: array of RealArray;
  minor_i: integer;
  minor_new_row: array of real;
  minor_j: integer;
begin
  minor_result_ := [];
  minor_i := 0;
  while minor_i < Length(matrix) do begin
  if minor_i <> row then begin
  minor_new_row := [];
  minor_j := 0;
  while minor_j < Length(matrix[minor_i]) do begin
  if minor_j <> column then begin
  minor_new_row := concat(minor_new_row, [matrix[minor_i][minor_j]]);
end;
  minor_j := minor_j + 1;
end;
  minor_result_ := concat(minor_result_, [minor_new_row]);
end;
  minor_i := minor_i + 1;
end;
  exit(minor_result_);
end;
function determinant(matrix: RealArrayArray): real;
var
  determinant_det: real;
  determinant_c: integer;
  determinant_sub: RealArrayArray;
  determinant_sign: real;
begin
  if Length(matrix) = 1 then begin
  exit(matrix[0][0]);
end;
  determinant_det := 0;
  determinant_c := 0;
  while determinant_c < Length(matrix[0]) do begin
  determinant_sub := minor(matrix, 0, determinant_c);
  if (determinant_c mod 2) = 0 then begin
  determinant_sign := 1;
end else begin
  determinant_sign := -1;
end;
  determinant_det := determinant_det + ((matrix[0][determinant_c] * determinant(determinant_sub)) * determinant_sign);
  determinant_c := determinant_c + 1;
end;
  exit(determinant_det);
end;
function inverse(matrix: RealArrayArray): RealArrayArray;
var
  inverse_det: real;
  inverse_size: integer;
  inverse_matrix_minor: array of RealArray;
  inverse_i: integer;
  inverse_row: array of real;
  inverse_j: integer;
  inverse_m: RealArrayArray;
  inverse_cofactors: array of RealArray;
  inverse_sign: real;
  inverse_adjugate: RealArrayArray;
begin
  inverse_det := determinant(matrix);
  if inverse_det = 0 then begin
  exit([]);
end;
  inverse_size := Length(matrix);
  inverse_matrix_minor := [];
  inverse_i := 0;
  while inverse_i < inverse_size do begin
  inverse_row := [];
  inverse_j := 0;
  while inverse_j < inverse_size do begin
  inverse_m := minor(matrix, inverse_i, inverse_j);
  inverse_row := concat(inverse_row, [determinant(inverse_m)]);
  inverse_j := inverse_j + 1;
end;
  inverse_matrix_minor := concat(inverse_matrix_minor, [inverse_row]);
  inverse_i := inverse_i + 1;
end;
  inverse_cofactors := [];
  inverse_i := 0;
  while inverse_i < inverse_size do begin
  inverse_row := [];
  inverse_j := 0;
  while inverse_j < inverse_size do begin
  if ((inverse_i + inverse_j) mod 2) = 0 then begin
  inverse_sign := 1;
end else begin
  inverse_sign := -1;
end;
  inverse_row := concat(inverse_row, [inverse_matrix_minor[inverse_i][inverse_j] * inverse_sign]);
  inverse_j := inverse_j + 1;
end;
  inverse_cofactors := concat(inverse_cofactors, [inverse_row]);
  inverse_i := inverse_i + 1;
end;
  inverse_adjugate := transpose(inverse_cofactors);
  exit(scalar_multiply(inverse_adjugate, 1 / inverse_det));
end;
procedure main();
var
  main_matrix_a: array of array of real;
  main_matrix_b: array of array of real;
  main_matrix_c: array of array of real;
  main_matrix_d: array of array of real;
begin
  main_matrix_a := [[12, 10], [3, 9]];
  main_matrix_b := [[3, 4], [7, 4]];
  main_matrix_c := [[11, 12, 13, 14], [21, 22, 23, 24], [31, 32, 33, 34], [41, 42, 43, 44]];
  main_matrix_d := [[3, 0, 2], [2, 0, -2], [0, 1, 1]];
  writeln(('Add Operation, add(matrix_a, matrix_b) = ' + list_list_real_to_str(add([main_matrix_a, main_matrix_b]))) + ' ' + #10 + '');
  writeln(('Multiply Operation, multiply(matrix_a, matrix_b) = ' + list_list_real_to_str(multiply(main_matrix_a, main_matrix_b))) + ' ' + #10 + '');
  writeln(('Identity: ' + list_list_real_to_str(identity(5))) + '' + #10 + '');
  writeln(((('Minor of ' + list_list_real_to_str(main_matrix_c)) + ' = ') + list_list_real_to_str(minor(main_matrix_c, 1, 2))) + ' ' + #10 + '');
  writeln(((('Determinant of ' + list_list_real_to_str(main_matrix_b)) + ' = ') + FloatToStr(determinant(main_matrix_b))) + ' ' + #10 + '');
  writeln(((('Inverse of ' + list_list_real_to_str(main_matrix_d)) + ' = ') + list_list_real_to_str(inverse(main_matrix_d))) + '' + #10 + '');
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
