{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  img: array of RealArray;
  result_: real;
  value: real;
  a: RealArrayArray;
  x: integer;
  kernel_size: integer;
  b: RealArrayArray;
  mat: RealArrayArray;
  variance: real;
  spatial_variance: real;
  intensity_variance: real;
  y: integer;
function abs(x: real): real; forward;
function sqrtApprox(x: real): real; forward;
function expApprox(x: real): real; forward;
function vec_gaussian(mat: RealArrayArray; variance: real): RealArrayArray; forward;
function get_slice(img: RealArrayArray; x: integer; y: integer; kernel_size: integer): RealArrayArray; forward;
function get_gauss_kernel(kernel_size: integer; spatial_variance: real): RealArrayArray; forward;
function elementwise_sub(mat: RealArrayArray; value: real): RealArrayArray; forward;
function elementwise_mul(a: RealArrayArray; b: RealArrayArray): RealArrayArray; forward;
function matrix_sum(mat: RealArrayArray): real; forward;
function bilateral_filter(img: RealArrayArray; spatial_variance: real; intensity_variance: real; kernel_size: integer): real; forward;
function abs(x: real): real;
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
  sqrtApprox_guess := x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 10 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function expApprox(x: real): real;
var
  expApprox_term: real;
  expApprox_sum: real;
  expApprox_n: integer;
begin
  expApprox_term := 1;
  expApprox_sum := 1;
  expApprox_n := 1;
  while expApprox_n < 10 do begin
  expApprox_term := (expApprox_term * x) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  exit(expApprox_sum);
end;
function vec_gaussian(mat: RealArrayArray; variance: real): RealArrayArray;
var
  vec_gaussian_i: integer;
  vec_gaussian_out: array of RealArray;
  vec_gaussian_row: array of real;
  vec_gaussian_j: integer;
  vec_gaussian_v: real;
  vec_gaussian_e: real;
begin
  vec_gaussian_i := 0;
  vec_gaussian_out := [];
  while vec_gaussian_i < Length(mat) do begin
  vec_gaussian_row := [];
  vec_gaussian_j := 0;
  while vec_gaussian_j < Length(mat[vec_gaussian_i]) do begin
  vec_gaussian_v := mat[vec_gaussian_i][vec_gaussian_j];
  vec_gaussian_e := -(vec_gaussian_v * vec_gaussian_v) / (2 * variance);
  vec_gaussian_row := concat(vec_gaussian_row, [expApprox(vec_gaussian_e)]);
  vec_gaussian_j := vec_gaussian_j + 1;
end;
  vec_gaussian_out := concat(vec_gaussian_out, [vec_gaussian_row]);
  vec_gaussian_i := vec_gaussian_i + 1;
end;
  exit(vec_gaussian_out);
end;
function get_slice(img: RealArrayArray; x: integer; y: integer; kernel_size: integer): RealArrayArray;
var
  get_slice_half: integer;
  get_slice_i: integer;
  get_slice_slice: array of RealArray;
  get_slice_row: array of real;
  get_slice_j: integer;
begin
  get_slice_half := kernel_size div 2;
  get_slice_i := x - get_slice_half;
  get_slice_slice := [];
  while get_slice_i <= (x + get_slice_half) do begin
  get_slice_row := [];
  get_slice_j := y - get_slice_half;
  while get_slice_j <= (y + get_slice_half) do begin
  get_slice_row := concat(get_slice_row, [img[get_slice_i][get_slice_j]]);
  get_slice_j := get_slice_j + 1;
end;
  get_slice_slice := concat(get_slice_slice, [get_slice_row]);
  get_slice_i := get_slice_i + 1;
end;
  exit(get_slice_slice);
end;
function get_gauss_kernel(kernel_size: integer; spatial_variance: real): RealArrayArray;
var
  get_gauss_kernel_arr: array of RealArray;
  get_gauss_kernel_i: integer;
  get_gauss_kernel_row: array of real;
  get_gauss_kernel_j: integer;
  get_gauss_kernel_di: real;
  get_gauss_kernel_dj: real;
  get_gauss_kernel_dist: real;
begin
  get_gauss_kernel_arr := [];
  get_gauss_kernel_i := 0;
  while get_gauss_kernel_i < kernel_size do begin
  get_gauss_kernel_row := [];
  get_gauss_kernel_j := 0;
  while get_gauss_kernel_j < kernel_size do begin
  get_gauss_kernel_di := Double(get_gauss_kernel_i - (kernel_size div 2));
  get_gauss_kernel_dj := Double(get_gauss_kernel_j - (kernel_size div 2));
  get_gauss_kernel_dist := sqrtApprox((get_gauss_kernel_di * get_gauss_kernel_di) + (get_gauss_kernel_dj * get_gauss_kernel_dj));
  get_gauss_kernel_row := concat(get_gauss_kernel_row, [get_gauss_kernel_dist]);
  get_gauss_kernel_j := get_gauss_kernel_j + 1;
end;
  get_gauss_kernel_arr := concat(get_gauss_kernel_arr, [get_gauss_kernel_row]);
  get_gauss_kernel_i := get_gauss_kernel_i + 1;
end;
  exit(vec_gaussian(get_gauss_kernel_arr, spatial_variance));
end;
function elementwise_sub(mat: RealArrayArray; value: real): RealArrayArray;
var
  elementwise_sub_res: array of RealArray;
  elementwise_sub_i: integer;
  elementwise_sub_row: array of real;
  elementwise_sub_j: integer;
begin
  elementwise_sub_res := [];
  elementwise_sub_i := 0;
  while elementwise_sub_i < Length(mat) do begin
  elementwise_sub_row := [];
  elementwise_sub_j := 0;
  while elementwise_sub_j < Length(mat[elementwise_sub_i]) do begin
  elementwise_sub_row := concat(elementwise_sub_row, [mat[elementwise_sub_i][elementwise_sub_j] - value]);
  elementwise_sub_j := elementwise_sub_j + 1;
end;
  elementwise_sub_res := concat(elementwise_sub_res, [elementwise_sub_row]);
  elementwise_sub_i := elementwise_sub_i + 1;
end;
  exit(elementwise_sub_res);
end;
function elementwise_mul(a: RealArrayArray; b: RealArrayArray): RealArrayArray;
var
  elementwise_mul_res: array of RealArray;
  elementwise_mul_i: integer;
  elementwise_mul_row: array of real;
  elementwise_mul_j: integer;
begin
  elementwise_mul_res := [];
  elementwise_mul_i := 0;
  while elementwise_mul_i < Length(a) do begin
  elementwise_mul_row := [];
  elementwise_mul_j := 0;
  while elementwise_mul_j < Length(a[elementwise_mul_i]) do begin
  elementwise_mul_row := concat(elementwise_mul_row, [a[elementwise_mul_i][elementwise_mul_j] * b[elementwise_mul_i][elementwise_mul_j]]);
  elementwise_mul_j := elementwise_mul_j + 1;
end;
  elementwise_mul_res := concat(elementwise_mul_res, [elementwise_mul_row]);
  elementwise_mul_i := elementwise_mul_i + 1;
end;
  exit(elementwise_mul_res);
end;
function matrix_sum(mat: RealArrayArray): real;
var
  matrix_sum_total: real;
  matrix_sum_i: integer;
  matrix_sum_j: integer;
begin
  matrix_sum_total := 0;
  matrix_sum_i := 0;
  while matrix_sum_i < Length(mat) do begin
  matrix_sum_j := 0;
  while matrix_sum_j < Length(mat[matrix_sum_i]) do begin
  matrix_sum_total := matrix_sum_total + mat[matrix_sum_i][matrix_sum_j];
  matrix_sum_j := matrix_sum_j + 1;
end;
  matrix_sum_i := matrix_sum_i + 1;
end;
  exit(matrix_sum_total);
end;
function bilateral_filter(img: RealArrayArray; spatial_variance: real; intensity_variance: real; kernel_size: integer): real;
var
  bilateral_filter_gauss_ker: RealArrayArray;
  bilateral_filter_img_s: array of RealArray;
  bilateral_filter_center: real;
  bilateral_filter_img_i: RealArrayArray;
  bilateral_filter_img_ig: RealArrayArray;
  bilateral_filter_weights: RealArrayArray;
  bilateral_filter_vals: RealArrayArray;
  bilateral_filter_sum_weights: real;
  bilateral_filter_val: real;
begin
  bilateral_filter_gauss_ker := get_gauss_kernel(kernel_size, spatial_variance);
  bilateral_filter_img_s := img;
  bilateral_filter_center := bilateral_filter_img_s[kernel_size div 2][kernel_size div 2];
  bilateral_filter_img_i := elementwise_sub(bilateral_filter_img_s, bilateral_filter_center);
  bilateral_filter_img_ig := vec_gaussian(bilateral_filter_img_i, intensity_variance);
  bilateral_filter_weights := elementwise_mul(bilateral_filter_gauss_ker, bilateral_filter_img_ig);
  bilateral_filter_vals := elementwise_mul(bilateral_filter_img_s, bilateral_filter_weights);
  bilateral_filter_sum_weights := matrix_sum(bilateral_filter_weights);
  bilateral_filter_val := 0;
  if bilateral_filter_sum_weights <> 0 then begin
  bilateral_filter_val := matrix_sum(bilateral_filter_vals) / bilateral_filter_sum_weights;
end;
  exit(bilateral_filter_val);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  img := [[0.2, 0.3, 0.4], [0.3, 0.4, 0.5], [0.4, 0.5, 0.6]];
  result_ := bilateral_filter(img, 1, 1, 3);
  writeln(result_);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
