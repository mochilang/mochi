{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type RealArrayArray = array of RealArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  image: IntArrayArray;
  mat: IntArrayArray;
  kernel: IntArrayArray;
  gray: RealArrayArray;
  rgb: IntArrayArrayArray;
function rgb_to_gray(rgb: IntArrayArrayArray): RealArrayArray; forward;
function gray_to_binary(gray: RealArrayArray): IntArrayArray; forward;
function dilation(image: IntArrayArray; kernel: IntArrayArray): IntArrayArray; forward;
procedure print_float_matrix(mat: RealArrayArray); forward;
procedure print_int_matrix(mat: IntArrayArray); forward;
procedure main(); forward;
function rgb_to_gray(rgb: IntArrayArrayArray): RealArrayArray;
var
  rgb_to_gray_result_: array of RealArray;
  rgb_to_gray_i: integer;
  rgb_to_gray_row: array of real;
  rgb_to_gray_j: integer;
  rgb_to_gray_r: integer;
  rgb_to_gray_g: integer;
  rgb_to_gray_b: integer;
  rgb_to_gray_gray: real;
begin
  rgb_to_gray_result_ := [];
  rgb_to_gray_i := 0;
  while rgb_to_gray_i < Length(rgb) do begin
  rgb_to_gray_row := [];
  rgb_to_gray_j := 0;
  while rgb_to_gray_j < Length(rgb[rgb_to_gray_i]) do begin
  rgb_to_gray_r := rgb[rgb_to_gray_i][rgb_to_gray_j][0];
  rgb_to_gray_g := rgb[rgb_to_gray_i][rgb_to_gray_j][1];
  rgb_to_gray_b := rgb[rgb_to_gray_i][rgb_to_gray_j][2];
  rgb_to_gray_gray := ((0.2989 * (1 * rgb_to_gray_r)) + (0.587 * (1 * rgb_to_gray_g))) + (0.114 * (1 * rgb_to_gray_b));
  rgb_to_gray_row := concat(rgb_to_gray_row, [rgb_to_gray_gray]);
  rgb_to_gray_j := rgb_to_gray_j + 1;
end;
  rgb_to_gray_result_ := concat(rgb_to_gray_result_, [rgb_to_gray_row]);
  rgb_to_gray_i := rgb_to_gray_i + 1;
end;
  exit(rgb_to_gray_result_);
end;
function gray_to_binary(gray: RealArrayArray): IntArrayArray;
var
  gray_to_binary_result_: array of IntArray;
  gray_to_binary_i: integer;
  gray_to_binary_row: array of integer;
  gray_to_binary_j: integer;
  gray_to_binary_v: real;
begin
  gray_to_binary_result_ := [];
  gray_to_binary_i := 0;
  while gray_to_binary_i < Length(gray) do begin
  gray_to_binary_row := [];
  gray_to_binary_j := 0;
  while gray_to_binary_j < Length(gray[gray_to_binary_i]) do begin
  gray_to_binary_v := gray[gray_to_binary_i][gray_to_binary_j];
  if (gray_to_binary_v > 127) and (gray_to_binary_v <= 255) then begin
  gray_to_binary_row := concat(gray_to_binary_row, IntArray([1]));
end else begin
  gray_to_binary_row := concat(gray_to_binary_row, IntArray([0]));
end;
  gray_to_binary_j := gray_to_binary_j + 1;
end;
  gray_to_binary_result_ := concat(gray_to_binary_result_, [gray_to_binary_row]);
  gray_to_binary_i := gray_to_binary_i + 1;
end;
  exit(gray_to_binary_result_);
end;
function dilation(image: IntArrayArray; kernel: IntArrayArray): IntArrayArray;
var
  dilation_img_h: integer;
  dilation_img_w: integer;
  dilation_k_h: integer;
  dilation_k_w: integer;
  dilation_pad_h: integer;
  dilation_pad_w: integer;
  dilation_p_h: integer;
  dilation_p_w: integer;
  dilation_padded: array of IntArray;
  dilation_i: integer;
  dilation_row: array of integer;
  dilation_j: integer;
  dilation_output: array of IntArray;
  dilation_sum: integer;
  dilation_ky: integer;
  dilation_kx: integer;
begin
  dilation_img_h := Length(image);
  dilation_img_w := Length(image[0]);
  dilation_k_h := Length(kernel);
  dilation_k_w := Length(kernel[0]);
  dilation_pad_h := dilation_k_h div 2;
  dilation_pad_w := dilation_k_w div 2;
  dilation_p_h := dilation_img_h + (2 * dilation_pad_h);
  dilation_p_w := dilation_img_w + (2 * dilation_pad_w);
  dilation_padded := [];
  dilation_i := 0;
  while dilation_i < dilation_p_h do begin
  dilation_row := [];
  dilation_j := 0;
  while dilation_j < dilation_p_w do begin
  dilation_row := concat(dilation_row, IntArray([0]));
  dilation_j := dilation_j + 1;
end;
  dilation_padded := concat(dilation_padded, [dilation_row]);
  dilation_i := dilation_i + 1;
end;
  dilation_i := 0;
  while dilation_i < dilation_img_h do begin
  dilation_j := 0;
  while dilation_j < dilation_img_w do begin
  dilation_padded[dilation_pad_h + dilation_i][dilation_pad_w + dilation_j] := image[dilation_i][dilation_j];
  dilation_j := dilation_j + 1;
end;
  dilation_i := dilation_i + 1;
end;
  dilation_output := [];
  dilation_i := 0;
  while dilation_i < dilation_img_h do begin
  dilation_row := [];
  dilation_j := 0;
  while dilation_j < dilation_img_w do begin
  dilation_sum := 0;
  dilation_ky := 0;
  while dilation_ky < dilation_k_h do begin
  dilation_kx := 0;
  while dilation_kx < dilation_k_w do begin
  if kernel[dilation_ky][dilation_kx] = 1 then begin
  dilation_sum := dilation_sum + dilation_padded[dilation_i + dilation_ky][dilation_j + dilation_kx];
end;
  dilation_kx := dilation_kx + 1;
end;
  dilation_ky := dilation_ky + 1;
end;
  if dilation_sum > 0 then begin
  dilation_row := concat(dilation_row, IntArray([1]));
end else begin
  dilation_row := concat(dilation_row, IntArray([0]));
end;
  dilation_j := dilation_j + 1;
end;
  dilation_output := concat(dilation_output, [dilation_row]);
  dilation_i := dilation_i + 1;
end;
  exit(dilation_output);
end;
procedure print_float_matrix(mat: RealArrayArray);
var
  print_float_matrix_i: integer;
  print_float_matrix_line: string;
  print_float_matrix_j: integer;
begin
  print_float_matrix_i := 0;
  while print_float_matrix_i < Length(mat) do begin
  print_float_matrix_line := '';
  print_float_matrix_j := 0;
  while print_float_matrix_j < Length(mat[print_float_matrix_i]) do begin
  print_float_matrix_line := print_float_matrix_line + FloatToStr(mat[print_float_matrix_i][print_float_matrix_j]);
  if print_float_matrix_j < (Length(mat[print_float_matrix_i]) - 1) then begin
  print_float_matrix_line := print_float_matrix_line + ' ';
end;
  print_float_matrix_j := print_float_matrix_j + 1;
end;
  writeln(print_float_matrix_line);
  print_float_matrix_i := print_float_matrix_i + 1;
end;
end;
procedure print_int_matrix(mat: IntArrayArray);
var
  print_int_matrix_i: integer;
  print_int_matrix_line: string;
  print_int_matrix_j: integer;
begin
  print_int_matrix_i := 0;
  while print_int_matrix_i < Length(mat) do begin
  print_int_matrix_line := '';
  print_int_matrix_j := 0;
  while print_int_matrix_j < Length(mat[print_int_matrix_i]) do begin
  print_int_matrix_line := print_int_matrix_line + IntToStr(mat[print_int_matrix_i][print_int_matrix_j]);
  if print_int_matrix_j < (Length(mat[print_int_matrix_i]) - 1) then begin
  print_int_matrix_line := print_int_matrix_line + ' ';
end;
  print_int_matrix_j := print_int_matrix_j + 1;
end;
  writeln(print_int_matrix_line);
  print_int_matrix_i := print_int_matrix_i + 1;
end;
end;
procedure main();
var
  main_rgb_example: array of array of array of integer;
  main_gray_example: array of array of real;
  main_binary_image: array of array of integer;
  main_kernel: array of array of integer;
begin
  main_rgb_example := [[[127, 255, 0]]];
  print_float_matrix(rgb_to_gray(main_rgb_example));
  main_gray_example := [[26, 255, 14], [5, 147, 20], [1, 200, 0]];
  print_int_matrix(gray_to_binary(main_gray_example));
  main_binary_image := [[0, 1, 0], [0, 1, 0], [0, 1, 0]];
  main_kernel := [[0, 1, 0], [1, 1, 1], [0, 1, 0]];
  print_int_matrix(dilation(main_binary_image, main_kernel));
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
