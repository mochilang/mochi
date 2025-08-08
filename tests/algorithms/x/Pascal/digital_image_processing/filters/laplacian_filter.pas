{$mode objfpc}
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
  image: array of IntArray;
  result_: array of IntArray;
  r: integer;
  row_str: string;
  c: integer;
  src: IntArrayArray;
  ksize: integer;
  value: integer;
  rows: integer;
  cols: integer;
function make_matrix(rows: integer; cols: integer; value: integer): IntArrayArray; forward;
function my_laplacian(src: IntArrayArray; ksize: integer): IntArrayArray; forward;
function make_matrix(rows: integer; cols: integer; value: integer): IntArrayArray;
var
  make_matrix_result_: array of IntArray;
  make_matrix_i: integer;
  make_matrix_row: array of integer;
  make_matrix_j: integer;
begin
  make_matrix_result_ := [];
  make_matrix_i := 0;
  while make_matrix_i < rows do begin
  make_matrix_row := [];
  make_matrix_j := 0;
  while make_matrix_j < cols do begin
  make_matrix_row := concat(make_matrix_row, IntArray([value]));
  make_matrix_j := make_matrix_j + 1;
end;
  make_matrix_result_ := concat(make_matrix_result_, [make_matrix_row]);
  make_matrix_i := make_matrix_i + 1;
end;
  exit(make_matrix_result_);
end;
function my_laplacian(src: IntArrayArray; ksize: integer): IntArrayArray;
var
  my_laplacian_kernel: array of IntArray;
  my_laplacian_rows: integer;
  my_laplacian_cols: integer;
  my_laplacian_k: integer;
  my_laplacian_pad: integer;
  my_laplacian_output: IntArrayArray;
  my_laplacian_i: integer;
  my_laplacian_j: integer;
  my_laplacian_sum: integer;
  my_laplacian_ki: integer;
  my_laplacian_kj: integer;
  my_laplacian_ii: integer;
  my_laplacian_jj: integer;
  my_laplacian_val: integer;
begin
  my_laplacian_kernel := [];
  if ksize = 1 then begin
  my_laplacian_kernel := [[0, -1, 0], [-1, 4, -1], [0, -1, 0]];
end else begin
  if ksize = 3 then begin
  my_laplacian_kernel := [[0, 1, 0], [1, -4, 1], [0, 1, 0]];
end else begin
  if ksize = 5 then begin
  my_laplacian_kernel := [[0, 0, -1, 0, 0], [0, -1, -2, -1, 0], [-1, -2, 16, -2, -1], [0, -1, -2, -1, 0], [0, 0, -1, 0, 0]];
end else begin
  if ksize = 7 then begin
  my_laplacian_kernel := [[0, 0, 0, -1, 0, 0, 0], [0, 0, -2, -3, -2, 0, 0], [0, -2, -7, -10, -7, -2, 0], [-1, -3, -10, 68, -10, -3, -1], [0, -2, -7, -10, -7, -2, 0], [0, 0, -2, -3, -2, 0, 0], [0, 0, 0, -1, 0, 0, 0]];
end else begin
  panic('ksize must be in (1, 3, 5, 7)');
end;
end;
end;
end;
  my_laplacian_rows := Length(src);
  my_laplacian_cols := Length(src[0]);
  my_laplacian_k := Length(my_laplacian_kernel);
  my_laplacian_pad := my_laplacian_k div 2;
  my_laplacian_output := make_matrix(my_laplacian_rows, my_laplacian_cols, 0);
  my_laplacian_i := 0;
  while my_laplacian_i < my_laplacian_rows do begin
  my_laplacian_j := 0;
  while my_laplacian_j < my_laplacian_cols do begin
  my_laplacian_sum := 0;
  my_laplacian_ki := 0;
  while my_laplacian_ki < my_laplacian_k do begin
  my_laplacian_kj := 0;
  while my_laplacian_kj < my_laplacian_k do begin
  my_laplacian_ii := (my_laplacian_i + my_laplacian_ki) - my_laplacian_pad;
  my_laplacian_jj := (my_laplacian_j + my_laplacian_kj) - my_laplacian_pad;
  my_laplacian_val := 0;
  if (((my_laplacian_ii >= 0) and (my_laplacian_ii < my_laplacian_rows)) and (my_laplacian_jj >= 0)) and (my_laplacian_jj < my_laplacian_cols) then begin
  my_laplacian_val := src[my_laplacian_ii][my_laplacian_jj];
end;
  my_laplacian_sum := my_laplacian_sum + (my_laplacian_val * my_laplacian_kernel[my_laplacian_ki][my_laplacian_kj]);
  my_laplacian_kj := my_laplacian_kj + 1;
end;
  my_laplacian_ki := my_laplacian_ki + 1;
end;
  my_laplacian_output[my_laplacian_i][my_laplacian_j] := my_laplacian_sum;
  my_laplacian_j := my_laplacian_j + 1;
end;
  my_laplacian_i := my_laplacian_i + 1;
end;
  exit(my_laplacian_output);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  image := [[0, 0, 0, 0, 0], [0, 10, 10, 10, 0], [0, 10, 10, 10, 0], [0, 10, 10, 10, 0], [0, 0, 0, 0, 0]];
  result_ := my_laplacian(image, 3);
  r := 0;
  while r < Length(result_) do begin
  row_str := '[';
  c := 0;
  while c < Length(result_[r]) do begin
  row_str := row_str + IntToStr(result_[r][c]);
  if (c + 1) < Length(result_[r]) then begin
  row_str := row_str + ', ';
end;
  c := c + 1;
end;
  row_str := row_str + ']';
  writeln(row_str);
  r := r + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
