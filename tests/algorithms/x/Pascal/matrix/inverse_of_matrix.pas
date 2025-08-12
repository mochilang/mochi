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
procedure show_list_real(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
procedure show_list_list_real(xs: array of RealArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list_real(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  m2: array of RealArray;
  m3: array of RealArray;
  matrix: RealArrayArray;
function inverse_of_matrix(matrix: RealArrayArray): RealArrayArray; forward;
function inverse_of_matrix(matrix: RealArrayArray): RealArrayArray;
var
  inverse_of_matrix_det: real;
  inverse_of_matrix_cof: array of RealArray;
  inverse_of_matrix_inv: array of RealArray;
  inverse_of_matrix_i: integer;
  inverse_of_matrix_j: integer;
begin
  if ((Length(matrix) = 2) and (Length(matrix[0]) = 2)) and (Length(matrix[1]) = 2) then begin
  inverse_of_matrix_det := (matrix[0][0] * matrix[1][1]) - (matrix[1][0] * matrix[0][1]);
  if inverse_of_matrix_det = 0 then begin
  writeln('This matrix has no inverse.');
  exit([]);
end;
  exit([[matrix[1][1] / inverse_of_matrix_det, -matrix[0][1] / inverse_of_matrix_det], [-matrix[1][0] / inverse_of_matrix_det, matrix[0][0] / inverse_of_matrix_det]]);
end else begin
  if (((Length(matrix) = 3) and (Length(matrix[0]) = 3)) and (Length(matrix[1]) = 3)) and (Length(matrix[2]) = 3) then begin
  inverse_of_matrix_det := ((((matrix[0][0] * matrix[1][1]) * matrix[2][2]) + ((matrix[0][1] * matrix[1][2]) * matrix[2][0])) + ((matrix[0][2] * matrix[1][0]) * matrix[2][1])) - ((((matrix[0][2] * matrix[1][1]) * matrix[2][0]) + ((matrix[0][1] * matrix[1][0]) * matrix[2][2])) + ((matrix[0][0] * matrix[1][2]) * matrix[2][1]));
  if inverse_of_matrix_det = 0 then begin
  writeln('This matrix has no inverse.');
  exit([]);
end;
  inverse_of_matrix_cof := [[0, 0, 0], [0, 0, 0], [0, 0, 0]];
  inverse_of_matrix_cof[0][0] := (matrix[1][1] * matrix[2][2]) - (matrix[1][2] * matrix[2][1]);
  inverse_of_matrix_cof[0][1] := -((matrix[1][0] * matrix[2][2]) - (matrix[1][2] * matrix[2][0]));
  inverse_of_matrix_cof[0][2] := (matrix[1][0] * matrix[2][1]) - (matrix[1][1] * matrix[2][0]);
  inverse_of_matrix_cof[1][0] := -((matrix[0][1] * matrix[2][2]) - (matrix[0][2] * matrix[2][1]));
  inverse_of_matrix_cof[1][1] := (matrix[0][0] * matrix[2][2]) - (matrix[0][2] * matrix[2][0]);
  inverse_of_matrix_cof[1][2] := -((matrix[0][0] * matrix[2][1]) - (matrix[0][1] * matrix[2][0]));
  inverse_of_matrix_cof[2][0] := (matrix[0][1] * matrix[1][2]) - (matrix[0][2] * matrix[1][1]);
  inverse_of_matrix_cof[2][1] := -((matrix[0][0] * matrix[1][2]) - (matrix[0][2] * matrix[1][0]));
  inverse_of_matrix_cof[2][2] := (matrix[0][0] * matrix[1][1]) - (matrix[0][1] * matrix[1][0]);
  inverse_of_matrix_inv := [[0, 0, 0], [0, 0, 0], [0, 0, 0]];
  inverse_of_matrix_i := 0;
  while inverse_of_matrix_i < 3 do begin
  inverse_of_matrix_j := 0;
  while inverse_of_matrix_j < 3 do begin
  inverse_of_matrix_inv[inverse_of_matrix_i][inverse_of_matrix_j] := inverse_of_matrix_cof[inverse_of_matrix_j][inverse_of_matrix_i] / inverse_of_matrix_det;
  inverse_of_matrix_j := inverse_of_matrix_j + 1;
end;
  inverse_of_matrix_i := inverse_of_matrix_i + 1;
end;
  exit(inverse_of_matrix_inv);
end;
end;
  writeln('Please provide a matrix of size 2x2 or 3x3.');
  exit([]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  m2 := [[2, 5], [2, 0]];
  show_list_list_real(inverse_of_matrix(m2));
  m3 := [[2, 5, 7], [2, 0, 1], [1, 2, 3]];
  show_list_list_real(inverse_of_matrix(m3));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
