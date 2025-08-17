{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
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
procedure check_matrix(check_matrix_mat: RealArrayArray); forward;
function add(add_a: RealArrayArray; add_b: RealArrayArray): RealArrayArray; forward;
function subtract(subtract_a: RealArrayArray; subtract_b: RealArrayArray): RealArrayArray; forward;
function scalar_multiply(scalar_multiply_a: RealArrayArray; scalar_multiply_s: real): RealArrayArray; forward;
function multiply(multiply_a: RealArrayArray; multiply_b: RealArrayArray): RealArrayArray; forward;
function identity(identity_n: integer): RealArrayArray; forward;
function transpose(transpose_a: RealArrayArray): RealArrayArray; forward;
procedure main(); forward;
procedure check_matrix(check_matrix_mat: RealArrayArray);
begin
  if (Length(check_matrix_mat) < 2) or (Length(check_matrix_mat[0]) < 2) then begin
  panic('Expected a matrix with at least 2x2 dimensions');
end;
end;
function add(add_a: RealArrayArray; add_b: RealArrayArray): RealArrayArray;
var
  add_rows: integer;
  add_cols: integer;
  add_result_: array of RealArray;
  add_i: integer;
  add_row: array of real;
  add_j: integer;
begin
  check_matrix(add_a);
  check_matrix(add_b);
  if (Length(add_a) <> Length(add_b)) or (Length(add_a[0]) <> Length(add_b[0])) then begin
  panic('Matrices must have the same dimensions');
end;
  add_rows := Length(add_a);
  add_cols := Length(add_a[0]);
  add_result_ := [];
  add_i := 0;
  while add_i < add_rows do begin
  add_row := [];
  add_j := 0;
  while add_j < add_cols do begin
  add_row := concat(add_row, [add_a[add_i][add_j] + add_b[add_i][add_j]]);
  add_j := add_j + 1;
end;
  add_result_ := concat(add_result_, [add_row]);
  add_i := add_i + 1;
end;
  exit(add_result_);
end;
function subtract(subtract_a: RealArrayArray; subtract_b: RealArrayArray): RealArrayArray;
var
  subtract_rows: integer;
  subtract_cols: integer;
  subtract_result_: array of RealArray;
  subtract_i: integer;
  subtract_row: array of real;
  subtract_j: integer;
begin
  check_matrix(subtract_a);
  check_matrix(subtract_b);
  if (Length(subtract_a) <> Length(subtract_b)) or (Length(subtract_a[0]) <> Length(subtract_b[0])) then begin
  panic('Matrices must have the same dimensions');
end;
  subtract_rows := Length(subtract_a);
  subtract_cols := Length(subtract_a[0]);
  subtract_result_ := [];
  subtract_i := 0;
  while subtract_i < subtract_rows do begin
  subtract_row := [];
  subtract_j := 0;
  while subtract_j < subtract_cols do begin
  subtract_row := concat(subtract_row, [subtract_a[subtract_i][subtract_j] - subtract_b[subtract_i][subtract_j]]);
  subtract_j := subtract_j + 1;
end;
  subtract_result_ := concat(subtract_result_, [subtract_row]);
  subtract_i := subtract_i + 1;
end;
  exit(subtract_result_);
end;
function scalar_multiply(scalar_multiply_a: RealArrayArray; scalar_multiply_s: real): RealArrayArray;
var
  scalar_multiply_rows: integer;
  scalar_multiply_cols: integer;
  scalar_multiply_result_: array of RealArray;
  scalar_multiply_i: integer;
  scalar_multiply_row: array of real;
  scalar_multiply_j: integer;
begin
  check_matrix(scalar_multiply_a);
  scalar_multiply_rows := Length(scalar_multiply_a);
  scalar_multiply_cols := Length(scalar_multiply_a[0]);
  scalar_multiply_result_ := [];
  scalar_multiply_i := 0;
  while scalar_multiply_i < scalar_multiply_rows do begin
  scalar_multiply_row := [];
  scalar_multiply_j := 0;
  while scalar_multiply_j < scalar_multiply_cols do begin
  scalar_multiply_row := concat(scalar_multiply_row, [scalar_multiply_a[scalar_multiply_i][scalar_multiply_j] * scalar_multiply_s]);
  scalar_multiply_j := scalar_multiply_j + 1;
end;
  scalar_multiply_result_ := concat(scalar_multiply_result_, [scalar_multiply_row]);
  scalar_multiply_i := scalar_multiply_i + 1;
end;
  exit(scalar_multiply_result_);
end;
function multiply(multiply_a: RealArrayArray; multiply_b: RealArrayArray): RealArrayArray;
var
  multiply_rows: integer;
  multiply_cols: integer;
  multiply_result_: array of RealArray;
  multiply_i: integer;
  multiply_row: array of real;
  multiply_j: integer;
  multiply_sum: real;
  multiply_k: integer;
begin
  check_matrix(multiply_a);
  check_matrix(multiply_b);
  if Length(multiply_a[0]) <> Length(multiply_b) then begin
  panic('Invalid dimensions for matrix multiplication');
end;
  multiply_rows := Length(multiply_a);
  multiply_cols := Length(multiply_b[0]);
  multiply_result_ := [];
  multiply_i := 0;
  while multiply_i < multiply_rows do begin
  multiply_row := [];
  multiply_j := 0;
  while multiply_j < multiply_cols do begin
  multiply_sum := 0;
  multiply_k := 0;
  while multiply_k < Length(multiply_b) do begin
  multiply_sum := multiply_sum + (multiply_a[multiply_i][multiply_k] * multiply_b[multiply_k][multiply_j]);
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
function identity(identity_n: integer): RealArrayArray;
var
  identity_result_: array of RealArray;
  identity_i: integer;
  identity_row: array of real;
  identity_j: integer;
begin
  identity_result_ := [];
  identity_i := 0;
  while identity_i < identity_n do begin
  identity_row := [];
  identity_j := 0;
  while identity_j < identity_n do begin
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
function transpose(transpose_a: RealArrayArray): RealArrayArray;
var
  transpose_rows: integer;
  transpose_cols: integer;
  transpose_result_: array of RealArray;
  transpose_j: integer;
  transpose_row: array of real;
  transpose_i: integer;
begin
  check_matrix(transpose_a);
  transpose_rows := Length(transpose_a);
  transpose_cols := Length(transpose_a[0]);
  transpose_result_ := [];
  transpose_j := 0;
  while transpose_j < transpose_cols do begin
  transpose_row := [];
  transpose_i := 0;
  while transpose_i < transpose_rows do begin
  transpose_row := concat(transpose_row, [transpose_a[transpose_i][transpose_j]]);
  transpose_i := transpose_i + 1;
end;
  transpose_result_ := concat(transpose_result_, [transpose_row]);
  transpose_j := transpose_j + 1;
end;
  exit(transpose_result_);
end;
procedure main();
var
  main_mat_a: array of array of real;
  main_mat_b: array of array of real;
  main_mat_c: array of array of real;
begin
  main_mat_a := [[12, 10], [3, 9]];
  main_mat_b := [[3, 4], [7, 4]];
  main_mat_c := [[3, 0, 2], [2, 0, -2], [0, 1, 1]];
  writeln(list_list_real_to_str(add(main_mat_a, main_mat_b)));
  writeln(list_list_real_to_str(subtract(main_mat_a, main_mat_b)));
  writeln(list_list_real_to_str(multiply(main_mat_a, main_mat_b)));
  writeln(list_list_real_to_str(scalar_multiply(main_mat_a, 3.5)));
  writeln(list_list_real_to_str(identity(5)));
  writeln(list_list_real_to_str(transpose(main_mat_c)));
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
