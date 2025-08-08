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
  image: array of array of integer;
  laplace_kernel: array of array of integer;
  result_: IntArrayArray;
  kernel: IntArrayArray;
  block_h: integer;
  pad_size: integer;
  matrix: IntArrayArray;
  a: IntArray;
  b: IntArray;
  m: IntArrayArray;
  block_w: integer;
function pad_edge(image: IntArrayArray; pad_size: integer): IntArrayArray; forward;
function im2col(image: IntArrayArray; block_h: integer; block_w: integer): IntArrayArray; forward;
function flatten(matrix: IntArrayArray): IntArray; forward;
function dot(a: IntArray; b: IntArray): integer; forward;
function img_convolve(image: IntArrayArray; kernel: IntArrayArray): IntArrayArray; forward;
procedure print_matrix(m: IntArrayArray); forward;
function pad_edge(image: IntArrayArray; pad_size: integer): IntArrayArray;
var
  pad_edge_height: integer;
  pad_edge_width: integer;
  pad_edge_new_height: integer;
  pad_edge_new_width: integer;
  pad_edge_padded: array of IntArray;
  pad_edge_i: integer;
  pad_edge_row: array of integer;
  pad_edge_src_i: integer;
  pad_edge_j: integer;
  pad_edge_src_j: integer;
begin
  pad_edge_height := Length(image);
  pad_edge_width := Length(image[0]);
  pad_edge_new_height := pad_edge_height + (pad_size * 2);
  pad_edge_new_width := pad_edge_width + (pad_size * 2);
  pad_edge_padded := [];
  pad_edge_i := 0;
  while pad_edge_i < pad_edge_new_height do begin
  pad_edge_row := [];
  pad_edge_src_i := pad_edge_i;
  if pad_edge_src_i < pad_size then begin
  pad_edge_src_i := 0;
end;
  if pad_edge_src_i >= (pad_edge_height + pad_size) then begin
  pad_edge_src_i := pad_edge_height - 1;
end else begin
  pad_edge_src_i := pad_edge_src_i - pad_size;
end;
  pad_edge_j := 0;
  while pad_edge_j < pad_edge_new_width do begin
  pad_edge_src_j := pad_edge_j;
  if pad_edge_src_j < pad_size then begin
  pad_edge_src_j := 0;
end;
  if pad_edge_src_j >= (pad_edge_width + pad_size) then begin
  pad_edge_src_j := pad_edge_width - 1;
end else begin
  pad_edge_src_j := pad_edge_src_j - pad_size;
end;
  pad_edge_row := concat(pad_edge_row, IntArray([image[pad_edge_src_i][pad_edge_src_j]]));
  pad_edge_j := pad_edge_j + 1;
end;
  pad_edge_padded := concat(pad_edge_padded, [pad_edge_row]);
  pad_edge_i := pad_edge_i + 1;
end;
  exit(pad_edge_padded);
end;
function im2col(image: IntArrayArray; block_h: integer; block_w: integer): IntArrayArray;
var
  im2col_rows: integer;
  im2col_cols: integer;
  im2col_dst_height: integer;
  im2col_dst_width: integer;
  im2col_image_array: array of IntArray;
  im2col_i: integer;
  im2col_j: integer;
  im2col_window: array of integer;
  im2col_bi: integer;
  im2col_bj: integer;
begin
  im2col_rows := Length(image);
  im2col_cols := Length(image[0]);
  im2col_dst_height := (im2col_rows - block_h) + 1;
  im2col_dst_width := (im2col_cols - block_w) + 1;
  im2col_image_array := [];
  im2col_i := 0;
  while im2col_i < im2col_dst_height do begin
  im2col_j := 0;
  while im2col_j < im2col_dst_width do begin
  im2col_window := [];
  im2col_bi := 0;
  while im2col_bi < block_h do begin
  im2col_bj := 0;
  while im2col_bj < block_w do begin
  im2col_window := concat(im2col_window, IntArray([image[im2col_i + im2col_bi][im2col_j + im2col_bj]]));
  im2col_bj := im2col_bj + 1;
end;
  im2col_bi := im2col_bi + 1;
end;
  im2col_image_array := concat(im2col_image_array, [im2col_window]);
  im2col_j := im2col_j + 1;
end;
  im2col_i := im2col_i + 1;
end;
  exit(im2col_image_array);
end;
function flatten(matrix: IntArrayArray): IntArray;
var
  flatten_out: array of integer;
  flatten_i: integer;
  flatten_j: integer;
begin
  flatten_out := [];
  flatten_i := 0;
  while flatten_i < Length(matrix) do begin
  flatten_j := 0;
  while flatten_j < Length(matrix[flatten_i]) do begin
  flatten_out := concat(flatten_out, IntArray([matrix[flatten_i][flatten_j]]));
  flatten_j := flatten_j + 1;
end;
  flatten_i := flatten_i + 1;
end;
  exit(flatten_out);
end;
function dot(a: IntArray; b: IntArray): integer;
var
  dot_sum: integer;
  dot_i: integer;
begin
  dot_sum := 0;
  dot_i := 0;
  while dot_i < Length(a) do begin
  dot_sum := dot_sum + (a[dot_i] * b[dot_i]);
  dot_i := dot_i + 1;
end;
  exit(dot_sum);
end;
function img_convolve(image: IntArrayArray; kernel: IntArrayArray): IntArrayArray;
var
  img_convolve_height: integer;
  img_convolve_width: integer;
  img_convolve_k_size: integer;
  img_convolve_pad_size: integer;
  img_convolve_padded: IntArrayArray;
  img_convolve_image_array: IntArrayArray;
  img_convolve_kernel_flat: IntArray;
  img_convolve_dst: array of IntArray;
  img_convolve_idx: integer;
  img_convolve_i: integer;
  img_convolve_row: array of integer;
  img_convolve_j: integer;
  img_convolve_val: integer;
begin
  img_convolve_height := Length(image);
  img_convolve_width := Length(image[0]);
  img_convolve_k_size := Length(kernel);
  img_convolve_pad_size := img_convolve_k_size div 2;
  img_convolve_padded := pad_edge(image, img_convolve_pad_size);
  img_convolve_image_array := im2col(img_convolve_padded, img_convolve_k_size, img_convolve_k_size);
  img_convolve_kernel_flat := flatten(kernel);
  img_convolve_dst := [];
  img_convolve_idx := 0;
  img_convolve_i := 0;
  while img_convolve_i < img_convolve_height do begin
  img_convolve_row := [];
  img_convolve_j := 0;
  while img_convolve_j < img_convolve_width do begin
  img_convolve_val := dot(img_convolve_image_array[img_convolve_idx], img_convolve_kernel_flat);
  img_convolve_row := concat(img_convolve_row, IntArray([img_convolve_val]));
  img_convolve_idx := img_convolve_idx + 1;
  img_convolve_j := img_convolve_j + 1;
end;
  img_convolve_dst := concat(img_convolve_dst, [img_convolve_row]);
  img_convolve_i := img_convolve_i + 1;
end;
  exit(img_convolve_dst);
end;
procedure print_matrix(m: IntArrayArray);
var
  print_matrix_i: integer;
  print_matrix_line: string;
  print_matrix_j: integer;
begin
  print_matrix_i := 0;
  while print_matrix_i < Length(m) do begin
  print_matrix_line := '';
  print_matrix_j := 0;
  while print_matrix_j < Length(m[print_matrix_i]) do begin
  if print_matrix_j > 0 then begin
  print_matrix_line := print_matrix_line + ' ';
end;
  print_matrix_line := print_matrix_line + IntToStr(m[print_matrix_i][print_matrix_j]);
  print_matrix_j := print_matrix_j + 1;
end;
  writeln(print_matrix_line);
  print_matrix_i := print_matrix_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  image := [[1, 2, 3, 0, 0], [4, 5, 6, 0, 0], [7, 8, 9, 0, 0], [0, 0, 0, 0, 0], [0, 0, 0, 0, 0]];
  laplace_kernel := [[0, 1, 0], [1, -4, 1], [0, 1, 0]];
  result_ := img_convolve(image, laplace_kernel);
  print_matrix(result_);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
