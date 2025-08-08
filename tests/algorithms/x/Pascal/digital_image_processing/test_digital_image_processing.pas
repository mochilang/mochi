{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
  img: array of IntArray;
  negative: IntArrayArray;
  contrast: IntArrayArray;
  kernel: RealArrayArray;
  laplace: array of RealArray;
  convolved: IntArrayArray;
  medianed: IntArrayArray;
  sobel: IntArrayArray;
  lbp_img: IntArrayArray;
  xs: IntArray;
  k: integer;
  factor: integer;
  n: integer;
  e: integer;
  x: integer;
  sigma: real;
  y: integer;
function clamp_byte(x: integer): integer; forward;
function convert_to_negative(img: IntArrayArray): IntArrayArray; forward;
function change_contrast(img: IntArrayArray; factor: integer): IntArrayArray; forward;
function gen_gaussian_kernel(n: integer; sigma: real): RealArrayArray; forward;
function img_convolve(img: IntArrayArray; kernel: RealArrayArray): IntArrayArray; forward;
function sort_ints(xs: IntArray): IntArray; forward;
function median_filter(img: IntArrayArray; k: integer): IntArrayArray; forward;
function iabs(x: integer): integer; forward;
function sobel_filter(img: IntArrayArray): IntArrayArray; forward;
function get_neighbors_pixel(img: IntArrayArray; x: integer; y: integer): IntArray; forward;
function pow2(e: integer): integer; forward;
function local_binary_value(img: IntArrayArray; x: integer; y: integer): integer; forward;
function local_binary_pattern(img: IntArrayArray): IntArrayArray; forward;
function clamp_byte(x: integer): integer;
begin
  if x < 0 then begin
  exit(0);
end;
  if x > 255 then begin
  exit(255);
end;
  exit(x);
end;
function convert_to_negative(img: IntArrayArray): IntArrayArray;
var
  convert_to_negative_h: integer;
  convert_to_negative_w: integer;
  convert_to_negative_out: array of IntArray;
  convert_to_negative_y: integer;
  convert_to_negative_row: array of integer;
  convert_to_negative_x: integer;
begin
  convert_to_negative_h := Length(img);
  convert_to_negative_w := Length(img[0]);
  convert_to_negative_out := [];
  convert_to_negative_y := 0;
  while convert_to_negative_y < convert_to_negative_h do begin
  convert_to_negative_row := [];
  convert_to_negative_x := 0;
  while convert_to_negative_x < convert_to_negative_w do begin
  convert_to_negative_row := concat(convert_to_negative_row, IntArray([255 - img[convert_to_negative_y][convert_to_negative_x]]));
  convert_to_negative_x := convert_to_negative_x + 1;
end;
  convert_to_negative_out := concat(convert_to_negative_out, [convert_to_negative_row]);
  convert_to_negative_y := convert_to_negative_y + 1;
end;
  exit(convert_to_negative_out);
end;
function change_contrast(img: IntArrayArray; factor: integer): IntArrayArray;
var
  change_contrast_h: integer;
  change_contrast_w: integer;
  change_contrast_out: array of IntArray;
  change_contrast_y: integer;
  change_contrast_row: array of integer;
  change_contrast_x: integer;
  change_contrast_p: integer;
  change_contrast_v: integer;
begin
  change_contrast_h := Length(img);
  change_contrast_w := Length(img[0]);
  change_contrast_out := [];
  change_contrast_y := 0;
  while change_contrast_y < change_contrast_h do begin
  change_contrast_row := [];
  change_contrast_x := 0;
  while change_contrast_x < change_contrast_w do begin
  change_contrast_p := img[change_contrast_y][change_contrast_x];
  change_contrast_v := (((change_contrast_p - 128) * factor) div 100) + 128;
  change_contrast_v := clamp_byte(change_contrast_v);
  change_contrast_row := concat(change_contrast_row, IntArray([change_contrast_v]));
  change_contrast_x := change_contrast_x + 1;
end;
  change_contrast_out := concat(change_contrast_out, [change_contrast_row]);
  change_contrast_y := change_contrast_y + 1;
end;
  exit(change_contrast_out);
end;
function gen_gaussian_kernel(n: integer; sigma: real): RealArrayArray;
var
  gen_gaussian_kernel_k: array of RealArray;
  gen_gaussian_kernel_i: integer;
  gen_gaussian_kernel_row: array of real;
  gen_gaussian_kernel_j: integer;
begin
  if n = 3 then begin
  exit([[1 / 16, 2 / 16, 1 / 16], [2 / 16, 4 / 16, 2 / 16], [1 / 16, 2 / 16, 1 / 16]]);
end;
  gen_gaussian_kernel_k := [];
  gen_gaussian_kernel_i := 0;
  while gen_gaussian_kernel_i < n do begin
  gen_gaussian_kernel_row := [];
  gen_gaussian_kernel_j := 0;
  while gen_gaussian_kernel_j < n do begin
  gen_gaussian_kernel_row := concat(gen_gaussian_kernel_row, [0]);
  gen_gaussian_kernel_j := gen_gaussian_kernel_j + 1;
end;
  gen_gaussian_kernel_k := concat(gen_gaussian_kernel_k, [gen_gaussian_kernel_row]);
  gen_gaussian_kernel_i := gen_gaussian_kernel_i + 1;
end;
  exit(gen_gaussian_kernel_k);
end;
function img_convolve(img: IntArrayArray; kernel: RealArrayArray): IntArrayArray;
var
  img_convolve_h: integer;
  img_convolve_w: integer;
  img_convolve_out: array of IntArray;
  img_convolve_y: integer;
  img_convolve_row: array of integer;
  img_convolve_x: integer;
  img_convolve_acc: real;
  img_convolve_ky: integer;
  img_convolve_kx: integer;
  img_convolve_iy: integer;
  img_convolve_ix: integer;
  img_convolve_pixel: integer;
begin
  img_convolve_h := Length(img);
  img_convolve_w := Length(img[0]);
  img_convolve_out := [];
  img_convolve_y := 0;
  while img_convolve_y < img_convolve_h do begin
  img_convolve_row := [];
  img_convolve_x := 0;
  while img_convolve_x < img_convolve_w do begin
  img_convolve_acc := 0;
  img_convolve_ky := 0;
  while img_convolve_ky < Length(kernel) do begin
  img_convolve_kx := 0;
  while img_convolve_kx < Length(kernel[0]) do begin
  img_convolve_iy := (img_convolve_y + img_convolve_ky) - 1;
  img_convolve_ix := (img_convolve_x + img_convolve_kx) - 1;
  img_convolve_pixel := 0;
  if (((img_convolve_iy >= 0) and (img_convolve_iy < img_convolve_h)) and (img_convolve_ix >= 0)) and (img_convolve_ix < img_convolve_w) then begin
  img_convolve_pixel := img[img_convolve_iy][img_convolve_ix];
end;
  img_convolve_acc := img_convolve_acc + (kernel[img_convolve_ky][img_convolve_kx] * (1 * img_convolve_pixel));
  img_convolve_kx := img_convolve_kx + 1;
end;
  img_convolve_ky := img_convolve_ky + 1;
end;
  img_convolve_row := concat(img_convolve_row, IntArray([Trunc(img_convolve_acc)]));
  img_convolve_x := img_convolve_x + 1;
end;
  img_convolve_out := concat(img_convolve_out, [img_convolve_row]);
  img_convolve_y := img_convolve_y + 1;
end;
  exit(img_convolve_out);
end;
function sort_ints(xs: IntArray): IntArray;
var
  sort_ints_arr: array of integer;
  sort_ints_i: integer;
  sort_ints_j: integer;
  sort_ints_tmp: integer;
begin
  sort_ints_arr := xs;
  sort_ints_i := 0;
  while sort_ints_i < Length(sort_ints_arr) do begin
  sort_ints_j := 0;
  while sort_ints_j < ((Length(sort_ints_arr) - 1) - sort_ints_i) do begin
  if sort_ints_arr[sort_ints_j] > sort_ints_arr[sort_ints_j + 1] then begin
  sort_ints_tmp := sort_ints_arr[sort_ints_j];
  sort_ints_arr[sort_ints_j] := sort_ints_arr[sort_ints_j + 1];
  sort_ints_arr[sort_ints_j + 1] := sort_ints_tmp;
end;
  sort_ints_j := sort_ints_j + 1;
end;
  sort_ints_i := sort_ints_i + 1;
end;
  exit(sort_ints_arr);
end;
function median_filter(img: IntArrayArray; k: integer): IntArrayArray;
var
  median_filter_h: integer;
  median_filter_w: integer;
  median_filter_offset: integer;
  median_filter_out: array of IntArray;
  median_filter_y: integer;
  median_filter_row: array of integer;
  median_filter_x: integer;
  median_filter_vals: array of integer;
  median_filter_ky: integer;
  median_filter_kx: integer;
  median_filter_iy: integer;
  median_filter_ix: integer;
  median_filter_pixel: integer;
  median_filter_sorted: IntArray;
begin
  median_filter_h := Length(img);
  median_filter_w := Length(img[0]);
  median_filter_offset := k div 2;
  median_filter_out := [];
  median_filter_y := 0;
  while median_filter_y < median_filter_h do begin
  median_filter_row := [];
  median_filter_x := 0;
  while median_filter_x < median_filter_w do begin
  median_filter_vals := [];
  median_filter_ky := 0;
  while median_filter_ky < k do begin
  median_filter_kx := 0;
  while median_filter_kx < k do begin
  median_filter_iy := (median_filter_y + median_filter_ky) - median_filter_offset;
  median_filter_ix := (median_filter_x + median_filter_kx) - median_filter_offset;
  median_filter_pixel := 0;
  if (((median_filter_iy >= 0) and (median_filter_iy < median_filter_h)) and (median_filter_ix >= 0)) and (median_filter_ix < median_filter_w) then begin
  median_filter_pixel := img[median_filter_iy][median_filter_ix];
end;
  median_filter_vals := concat(median_filter_vals, IntArray([median_filter_pixel]));
  median_filter_kx := median_filter_kx + 1;
end;
  median_filter_ky := median_filter_ky + 1;
end;
  median_filter_sorted := sort_ints(median_filter_vals);
  median_filter_row := concat(median_filter_row, IntArray([median_filter_sorted[Length(median_filter_sorted) div 2]]));
  median_filter_x := median_filter_x + 1;
end;
  median_filter_out := concat(median_filter_out, [median_filter_row]);
  median_filter_y := median_filter_y + 1;
end;
  exit(median_filter_out);
end;
function iabs(x: integer): integer;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sobel_filter(img: IntArrayArray): IntArrayArray;
var
  sobel_filter_gx: array of array of integer;
  sobel_filter_gy: array of array of integer;
  sobel_filter_h: integer;
  sobel_filter_w: integer;
  sobel_filter_out: array of IntArray;
  sobel_filter_y: integer;
  sobel_filter_row: array of integer;
  sobel_filter_x: integer;
  sobel_filter_sx: integer;
  sobel_filter_sy: integer;
  sobel_filter_ky: integer;
  sobel_filter_kx: integer;
  sobel_filter_iy: integer;
  sobel_filter_ix: integer;
  sobel_filter_pixel: integer;
begin
  sobel_filter_gx := [[1, 0, -1], [2, 0, -2], [1, 0, -1]];
  sobel_filter_gy := [[1, 2, 1], [0, 0, 0], [-1, -2, -1]];
  sobel_filter_h := Length(img);
  sobel_filter_w := Length(img[0]);
  sobel_filter_out := [];
  sobel_filter_y := 0;
  while sobel_filter_y < sobel_filter_h do begin
  sobel_filter_row := [];
  sobel_filter_x := 0;
  while sobel_filter_x < sobel_filter_w do begin
  sobel_filter_sx := 0;
  sobel_filter_sy := 0;
  sobel_filter_ky := 0;
  while sobel_filter_ky < 3 do begin
  sobel_filter_kx := 0;
  while sobel_filter_kx < 3 do begin
  sobel_filter_iy := (sobel_filter_y + sobel_filter_ky) - 1;
  sobel_filter_ix := (sobel_filter_x + sobel_filter_kx) - 1;
  sobel_filter_pixel := 0;
  if (((sobel_filter_iy >= 0) and (sobel_filter_iy < sobel_filter_h)) and (sobel_filter_ix >= 0)) and (sobel_filter_ix < sobel_filter_w) then begin
  sobel_filter_pixel := img[sobel_filter_iy][sobel_filter_ix];
end;
  sobel_filter_sx := sobel_filter_sx + (sobel_filter_gx[sobel_filter_ky][sobel_filter_kx] * sobel_filter_pixel);
  sobel_filter_sy := sobel_filter_sy + (sobel_filter_gy[sobel_filter_ky][sobel_filter_kx] * sobel_filter_pixel);
  sobel_filter_kx := sobel_filter_kx + 1;
end;
  sobel_filter_ky := sobel_filter_ky + 1;
end;
  sobel_filter_row := concat(sobel_filter_row, IntArray([iabs(sobel_filter_sx) + iabs(sobel_filter_sy)]));
  sobel_filter_x := sobel_filter_x + 1;
end;
  sobel_filter_out := concat(sobel_filter_out, [sobel_filter_row]);
  sobel_filter_y := sobel_filter_y + 1;
end;
  exit(sobel_filter_out);
end;
function get_neighbors_pixel(img: IntArrayArray; x: integer; y: integer): IntArray;
var
  get_neighbors_pixel_h: integer;
  get_neighbors_pixel_w: integer;
  get_neighbors_pixel_neighbors: array of integer;
  get_neighbors_pixel_dy: integer;
  get_neighbors_pixel_dx: integer;
  get_neighbors_pixel_ny: integer;
  get_neighbors_pixel_nx: integer;
  get_neighbors_pixel_val: integer;
begin
  get_neighbors_pixel_h := Length(img);
  get_neighbors_pixel_w := Length(img[0]);
  get_neighbors_pixel_neighbors := [];
  get_neighbors_pixel_dy := -1;
  while get_neighbors_pixel_dy <= 1 do begin
  get_neighbors_pixel_dx := -1;
  while get_neighbors_pixel_dx <= 1 do begin
  if not ((get_neighbors_pixel_dx = 0) and (get_neighbors_pixel_dy = 0)) then begin
  get_neighbors_pixel_ny := y + get_neighbors_pixel_dy;
  get_neighbors_pixel_nx := x + get_neighbors_pixel_dx;
  get_neighbors_pixel_val := 0;
  if (((get_neighbors_pixel_ny >= 0) and (get_neighbors_pixel_ny < get_neighbors_pixel_h)) and (get_neighbors_pixel_nx >= 0)) and (get_neighbors_pixel_nx < get_neighbors_pixel_w) then begin
  get_neighbors_pixel_val := img[get_neighbors_pixel_ny][get_neighbors_pixel_nx];
end;
  get_neighbors_pixel_neighbors := concat(get_neighbors_pixel_neighbors, IntArray([get_neighbors_pixel_val]));
end;
  get_neighbors_pixel_dx := get_neighbors_pixel_dx + 1;
end;
  get_neighbors_pixel_dy := get_neighbors_pixel_dy + 1;
end;
  exit(get_neighbors_pixel_neighbors);
end;
function pow2(e: integer): integer;
var
  pow2_r: integer;
  pow2_i: integer;
begin
  pow2_r := 1;
  pow2_i := 0;
  while pow2_i < e do begin
  pow2_r := pow2_r * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_r);
end;
function local_binary_value(img: IntArrayArray; x: integer; y: integer): integer;
var
  local_binary_value_center: integer;
  local_binary_value_neighbors: IntArray;
  local_binary_value_v: integer;
  local_binary_value_i: integer;
begin
  local_binary_value_center := img[y][x];
  local_binary_value_neighbors := get_neighbors_pixel(img, x, y);
  local_binary_value_v := 0;
  local_binary_value_i := 0;
  while local_binary_value_i < Length(local_binary_value_neighbors) do begin
  if local_binary_value_neighbors[local_binary_value_i] >= local_binary_value_center then begin
  local_binary_value_v := local_binary_value_v + pow2(local_binary_value_i);
end;
  local_binary_value_i := local_binary_value_i + 1;
end;
  exit(local_binary_value_v);
end;
function local_binary_pattern(img: IntArrayArray): IntArrayArray;
var
  local_binary_pattern_h: integer;
  local_binary_pattern_w: integer;
  local_binary_pattern_out: array of IntArray;
  local_binary_pattern_y: integer;
  local_binary_pattern_row: array of integer;
  local_binary_pattern_x: integer;
begin
  local_binary_pattern_h := Length(img);
  local_binary_pattern_w := Length(img[0]);
  local_binary_pattern_out := [];
  local_binary_pattern_y := 0;
  while local_binary_pattern_y < local_binary_pattern_h do begin
  local_binary_pattern_row := [];
  local_binary_pattern_x := 0;
  while local_binary_pattern_x < local_binary_pattern_w do begin
  local_binary_pattern_row := concat(local_binary_pattern_row, IntArray([local_binary_value(img, local_binary_pattern_x, local_binary_pattern_y)]));
  local_binary_pattern_x := local_binary_pattern_x + 1;
end;
  local_binary_pattern_out := concat(local_binary_pattern_out, [local_binary_pattern_row]);
  local_binary_pattern_y := local_binary_pattern_y + 1;
end;
  exit(local_binary_pattern_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  img := [[52, 55, 61], [62, 59, 55], [63, 65, 66]];
  negative := convert_to_negative(img);
  contrast := change_contrast(img, 110);
  kernel := gen_gaussian_kernel(3, 1);
  laplace := [[0.25, 0.5, 0.25], [0.5, -3, 0.5], [0.25, 0.5, 0.25]];
  convolved := img_convolve(img, laplace);
  medianed := median_filter(img, 3);
  sobel := sobel_filter(img);
  lbp_img := local_binary_pattern(img);
  show_list_list(negative);
  show_list_list(contrast);
  show_list(kernel);
  show_list_list(convolved);
  show_list_list(medianed);
  show_list_list(sobel);
  show_list_list(lbp_img);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
