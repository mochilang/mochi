{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  kernel: IntArrayArray;
  h: integer;
  mat: RealArrayArray;
  factor: real;
  pad: integer;
  img: RealArrayArray;
  y: real;
  w: integer;
  x: real;
  image: IntArrayArray;
function absf(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function atanApprox(x: real): real; forward;
function atan2Approx(y: real; x: real): real; forward;
function zeros(h: integer; w: integer): RealArrayArray; forward;
function pad_edge(img: RealArrayArray; pad: integer): RealArrayArray; forward;
function img_convolve(img: RealArrayArray; kernel: IntArrayArray): RealArrayArray; forward;
function abs_matrix(mat: RealArrayArray): RealArrayArray; forward;
function max_matrix(mat: RealArrayArray): real; forward;
function scale_matrix(mat: RealArrayArray; factor: real): RealArrayArray; forward;
function sobel_filter(image: IntArrayArray): RealArrayArrayArray; forward;
procedure print_matrix_int(mat: RealArrayArray); forward;
procedure print_matrix_float(mat: RealArrayArray); forward;
procedure main(); forward;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := x / 2;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function atanApprox(x: real): real;
begin
  if x > 1 then begin
  exit((PI / 2) - (x / ((x * x) + 0.28)));
end;
  if x < -1 then begin
  exit((-PI / 2) - (x / ((x * x) + 0.28)));
end;
  exit(x / (1 + ((0.28 * x) * x)));
end;
function atan2Approx(y: real; x: real): real;
var
  atan2Approx_a: real;
begin
  if x = 0 then begin
  if y > 0 then begin
  exit(PI / 2);
end;
  if y < 0 then begin
  exit(-PI / 2);
end;
  exit(0);
end;
  atan2Approx_a := atanApprox(y / x);
  if x > 0 then begin
  exit(atan2Approx_a);
end;
  if y >= 0 then begin
  exit(atan2Approx_a + PI);
end;
  exit(atan2Approx_a - PI);
end;
function zeros(h: integer; w: integer): RealArrayArray;
var
  zeros_m: array of RealArray;
  zeros_y: integer;
  zeros_row: array of real;
  zeros_x: integer;
begin
  zeros_m := [];
  zeros_y := 0;
  while zeros_y < h do begin
  zeros_row := [];
  zeros_x := 0;
  while zeros_x < w do begin
  zeros_row := concat(zeros_row, [0]);
  zeros_x := zeros_x + 1;
end;
  zeros_m := concat(zeros_m, [zeros_row]);
  zeros_y := zeros_y + 1;
end;
  exit(zeros_m);
end;
function pad_edge(img: RealArrayArray; pad: integer): RealArrayArray;
var
  pad_edge_h: integer;
  pad_edge_w: integer;
  pad_edge_out: RealArrayArray;
  pad_edge_y: integer;
  pad_edge_x: integer;
  pad_edge_sy: integer;
  pad_edge_sx: integer;
begin
  pad_edge_h := Length(img);
  pad_edge_w := Length(img[0]);
  pad_edge_out := zeros(pad_edge_h + (pad * 2), pad_edge_w + (pad * 2));
  pad_edge_y := 0;
  while pad_edge_y < (pad_edge_h + (pad * 2)) do begin
  pad_edge_x := 0;
  while pad_edge_x < (pad_edge_w + (pad * 2)) do begin
  pad_edge_sy := pad_edge_y - pad;
  if pad_edge_sy < 0 then begin
  pad_edge_sy := 0;
end;
  if pad_edge_sy >= pad_edge_h then begin
  pad_edge_sy := pad_edge_h - 1;
end;
  pad_edge_sx := pad_edge_x - pad;
  if pad_edge_sx < 0 then begin
  pad_edge_sx := 0;
end;
  if pad_edge_sx >= pad_edge_w then begin
  pad_edge_sx := pad_edge_w - 1;
end;
  pad_edge_out[pad_edge_y][pad_edge_x] := img[pad_edge_sy][pad_edge_sx];
  pad_edge_x := pad_edge_x + 1;
end;
  pad_edge_y := pad_edge_y + 1;
end;
  exit(pad_edge_out);
end;
function img_convolve(img: RealArrayArray; kernel: IntArrayArray): RealArrayArray;
var
  img_convolve_h: integer;
  img_convolve_w: integer;
  img_convolve_k: integer;
  img_convolve_pad: integer;
  img_convolve_padded: RealArrayArray;
  img_convolve_out: RealArrayArray;
  img_convolve_y: integer;
  img_convolve_x: integer;
  img_convolve_sum: real;
  img_convolve_i: integer;
  img_convolve_j: integer;
begin
  img_convolve_h := Length(img);
  img_convolve_w := Length(img[0]);
  img_convolve_k := Length(kernel);
  img_convolve_pad := img_convolve_k div 2;
  img_convolve_padded := pad_edge(img, img_convolve_pad);
  img_convolve_out := zeros(img_convolve_h, img_convolve_w);
  img_convolve_y := 0;
  while img_convolve_y < img_convolve_h do begin
  img_convolve_x := 0;
  while img_convolve_x < img_convolve_w do begin
  img_convolve_sum := 0;
  img_convolve_i := 0;
  while img_convolve_i < img_convolve_k do begin
  img_convolve_j := 0;
  while img_convolve_j < img_convolve_k do begin
  img_convolve_sum := img_convolve_sum + (img_convolve_padded[img_convolve_y + img_convolve_i][img_convolve_x + img_convolve_j] * Double(kernel[img_convolve_i][img_convolve_j]));
  img_convolve_j := img_convolve_j + 1;
end;
  img_convolve_i := img_convolve_i + 1;
end;
  img_convolve_out[img_convolve_y][img_convolve_x] := img_convolve_sum;
  img_convolve_x := img_convolve_x + 1;
end;
  img_convolve_y := img_convolve_y + 1;
end;
  exit(img_convolve_out);
end;
function abs_matrix(mat: RealArrayArray): RealArrayArray;
var
  abs_matrix_h: integer;
  abs_matrix_w: integer;
  abs_matrix_out: RealArrayArray;
  abs_matrix_y: integer;
  abs_matrix_x: integer;
  abs_matrix_v: real;
begin
  abs_matrix_h := Length(mat);
  abs_matrix_w := Length(mat[0]);
  abs_matrix_out := zeros(abs_matrix_h, abs_matrix_w);
  abs_matrix_y := 0;
  while abs_matrix_y < abs_matrix_h do begin
  abs_matrix_x := 0;
  while abs_matrix_x < abs_matrix_w do begin
  abs_matrix_v := mat[abs_matrix_y][abs_matrix_x];
  if abs_matrix_v < 0 then begin
  abs_matrix_out[abs_matrix_y][abs_matrix_x] := -abs_matrix_v;
end else begin
  abs_matrix_out[abs_matrix_y][abs_matrix_x] := abs_matrix_v;
end;
  abs_matrix_x := abs_matrix_x + 1;
end;
  abs_matrix_y := abs_matrix_y + 1;
end;
  exit(abs_matrix_out);
end;
function max_matrix(mat: RealArrayArray): real;
var
  max_matrix_max_val: real;
  max_matrix_y: integer;
  max_matrix_x: integer;
begin
  max_matrix_max_val := mat[0][0];
  max_matrix_y := 0;
  while max_matrix_y < Length(mat) do begin
  max_matrix_x := 0;
  while max_matrix_x < Length(mat[0]) do begin
  if mat[max_matrix_y][max_matrix_x] > max_matrix_max_val then begin
  max_matrix_max_val := mat[max_matrix_y][max_matrix_x];
end;
  max_matrix_x := max_matrix_x + 1;
end;
  max_matrix_y := max_matrix_y + 1;
end;
  exit(max_matrix_max_val);
end;
function scale_matrix(mat: RealArrayArray; factor: real): RealArrayArray;
var
  scale_matrix_h: integer;
  scale_matrix_w: integer;
  scale_matrix_out: RealArrayArray;
  scale_matrix_y: integer;
  scale_matrix_x: integer;
begin
  scale_matrix_h := Length(mat);
  scale_matrix_w := Length(mat[0]);
  scale_matrix_out := zeros(scale_matrix_h, scale_matrix_w);
  scale_matrix_y := 0;
  while scale_matrix_y < scale_matrix_h do begin
  scale_matrix_x := 0;
  while scale_matrix_x < scale_matrix_w do begin
  scale_matrix_out[scale_matrix_y][scale_matrix_x] := mat[scale_matrix_y][scale_matrix_x] * factor;
  scale_matrix_x := scale_matrix_x + 1;
end;
  scale_matrix_y := scale_matrix_y + 1;
end;
  exit(scale_matrix_out);
end;
function sobel_filter(image: IntArrayArray): RealArrayArrayArray;
var
  sobel_filter_h: integer;
  sobel_filter_w: integer;
  sobel_filter_img: array of RealArray;
  sobel_filter_y0: integer;
  sobel_filter_row: array of real;
  sobel_filter_x0: integer;
  sobel_filter_kernel_x: array of IntArray;
  sobel_filter_kernel_y: array of IntArray;
  sobel_filter_dst_x: RealArrayArray;
  sobel_filter_dst_y: RealArrayArray;
  sobel_filter_max_x: real;
  sobel_filter_max_y: real;
  sobel_filter_mag: RealArrayArray;
  sobel_filter_theta: RealArrayArray;
  sobel_filter_y: integer;
  sobel_filter_x: integer;
  sobel_filter_gx: real;
  sobel_filter_gy: real;
  sobel_filter_max_m: real;
begin
  sobel_filter_h := Length(image);
  sobel_filter_w := Length(image[0]);
  sobel_filter_img := [];
  sobel_filter_y0 := 0;
  while sobel_filter_y0 < sobel_filter_h do begin
  sobel_filter_row := [];
  sobel_filter_x0 := 0;
  while sobel_filter_x0 < sobel_filter_w do begin
  sobel_filter_row := concat(sobel_filter_row, [Double(image[sobel_filter_y0][sobel_filter_x0])]);
  sobel_filter_x0 := sobel_filter_x0 + 1;
end;
  sobel_filter_img := concat(sobel_filter_img, [sobel_filter_row]);
  sobel_filter_y0 := sobel_filter_y0 + 1;
end;
  sobel_filter_kernel_x := [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];
  sobel_filter_kernel_y := [[1, 2, 1], [0, 0, 0], [-1, -2, -1]];
  sobel_filter_dst_x := abs_matrix(img_convolve(sobel_filter_img, sobel_filter_kernel_x));
  sobel_filter_dst_y := abs_matrix(img_convolve(sobel_filter_img, sobel_filter_kernel_y));
  sobel_filter_max_x := max_matrix(sobel_filter_dst_x);
  sobel_filter_max_y := max_matrix(sobel_filter_dst_y);
  sobel_filter_dst_x := scale_matrix(sobel_filter_dst_x, 255 / sobel_filter_max_x);
  sobel_filter_dst_y := scale_matrix(sobel_filter_dst_y, 255 / sobel_filter_max_y);
  sobel_filter_mag := zeros(sobel_filter_h, sobel_filter_w);
  sobel_filter_theta := zeros(sobel_filter_h, sobel_filter_w);
  sobel_filter_y := 0;
  while sobel_filter_y < sobel_filter_h do begin
  sobel_filter_x := 0;
  while sobel_filter_x < sobel_filter_w do begin
  sobel_filter_gx := sobel_filter_dst_x[sobel_filter_y][sobel_filter_x];
  sobel_filter_gy := sobel_filter_dst_y[sobel_filter_y][sobel_filter_x];
  sobel_filter_mag[sobel_filter_y][sobel_filter_x] := sqrtApprox((sobel_filter_gx * sobel_filter_gx) + (sobel_filter_gy * sobel_filter_gy));
  sobel_filter_theta[sobel_filter_y][sobel_filter_x] := atan2Approx(sobel_filter_gy, sobel_filter_gx);
  sobel_filter_x := sobel_filter_x + 1;
end;
  sobel_filter_y := sobel_filter_y + 1;
end;
  sobel_filter_max_m := max_matrix(sobel_filter_mag);
  sobel_filter_mag := scale_matrix(sobel_filter_mag, 255 / sobel_filter_max_m);
  exit([sobel_filter_mag, sobel_filter_theta]);
end;
procedure print_matrix_int(mat: RealArrayArray);
var
  print_matrix_int_y: integer;
  print_matrix_int_line: string;
  print_matrix_int_x: integer;
begin
  print_matrix_int_y := 0;
  while print_matrix_int_y < Length(mat) do begin
  print_matrix_int_line := '';
  print_matrix_int_x := 0;
  while print_matrix_int_x < Length(mat[print_matrix_int_y]) do begin
  print_matrix_int_line := print_matrix_int_line + IntToStr(Trunc(mat[print_matrix_int_y][print_matrix_int_x]));
  if print_matrix_int_x < (Length(mat[print_matrix_int_y]) - 1) then begin
  print_matrix_int_line := print_matrix_int_line + ' ';
end;
  print_matrix_int_x := print_matrix_int_x + 1;
end;
  writeln(print_matrix_int_line);
  print_matrix_int_y := print_matrix_int_y + 1;
end;
end;
procedure print_matrix_float(mat: RealArrayArray);
var
  print_matrix_float_y: integer;
  print_matrix_float_line: string;
  print_matrix_float_x: integer;
begin
  print_matrix_float_y := 0;
  while print_matrix_float_y < Length(mat) do begin
  print_matrix_float_line := '';
  print_matrix_float_x := 0;
  while print_matrix_float_x < Length(mat[print_matrix_float_y]) do begin
  print_matrix_float_line := print_matrix_float_line + FloatToStr(mat[print_matrix_float_y][print_matrix_float_x]);
  if print_matrix_float_x < (Length(mat[print_matrix_float_y]) - 1) then begin
  print_matrix_float_line := print_matrix_float_line + ' ';
end;
  print_matrix_float_x := print_matrix_float_x + 1;
end;
  writeln(print_matrix_float_line);
  print_matrix_float_y := print_matrix_float_y + 1;
end;
end;
procedure main();
var
  main_img: array of IntArray;
  main_res: RealArrayArrayArray;
  main_mag: array of RealArray;
  main_theta: array of RealArray;
begin
  main_img := [[10, 10, 10, 10, 10], [10, 50, 50, 50, 10], [10, 50, 80, 50, 10], [10, 50, 50, 50, 10], [10, 10, 10, 10, 10]];
  main_res := sobel_filter(main_img);
  main_mag := main_res[0];
  main_theta := main_res[1];
  print_matrix_int(main_mag);
  print_matrix_float(main_theta);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
