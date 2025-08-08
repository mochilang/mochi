{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  GAUSSIAN_KERNEL: array of RealArray;
  SOBEL_GX: array of RealArray;
  SOBEL_GY: array of RealArray;
  canny_grad_idx: integer;
  canny_direction_idx: integer;
  image: array of RealArray;
  edges: RealArrayArray;
  high: real;
  low: real;
  grad: RealArrayArray;
  w: integer;
  rad: real;
  strong: real;
  x: real;
  y: real;
  h: integer;
  img: RealArrayArray;
  weak: real;
  kernel: RealArrayArray;
  direction: RealArrayArray;
function Map1(sobel_filter_dir: RealArrayArray; sobel_filter_grad: RealArrayArray): specialize TFPGMap<string, Variant>; forward;
function sqrtApprox(x: real): real; forward;
function atanApprox(x: real): real; forward;
function atan2Approx(y: real; x: real): real; forward;
function deg(rad: real): real; forward;
function zero_matrix(h: integer; w: integer): RealArrayArray; forward;
function convolve(img: RealArrayArray; kernel: RealArrayArray): RealArrayArray; forward;
function gaussian_blur(img: RealArrayArray): RealArrayArray; forward;
function sobel_filter(img: RealArrayArray): specialize TFPGMap<string, Variant>; forward;
function suppress_non_maximum(h: integer; w: integer; direction: RealArrayArray; grad: RealArrayArray): RealArrayArray; forward;
procedure double_threshold(h: integer; w: integer; img: RealArrayArray; low: real; high: real; weak: real; strong: real); forward;
procedure track_edge(h: integer; w: integer; img: RealArrayArray; weak: real; strong: real); forward;
function canny(image: RealArrayArray; low: real; high: real; weak: real; strong: real): RealArrayArray; forward;
procedure print_image(img: RealArrayArray); forward;
function Map1(sobel_filter_dir: RealArrayArray; sobel_filter_grad: RealArrayArray): specialize TFPGMap<string, Variant>;
var
  _ptr2: ^RealArrayArray;
  _ptr3: ^RealArrayArray;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  New(_ptr2);
  _ptr2^ := sobel_filter_grad;
  Result.AddOrSetData('grad', Variant(PtrUInt(_ptr2)));
  New(_ptr3);
  _ptr3^ := sobel_filter_dir;
  Result.AddOrSetData('dir', Variant(PtrUInt(_ptr3)));
end;
function sqrtApprox(x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
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
  atan2Approx_r: real;
begin
  if x > 0 then begin
  atan2Approx_r := atanApprox(y / x);
  exit(atan2Approx_r);
end;
  if x < 0 then begin
  if y >= 0 then begin
  exit(atanApprox(y / x) + PI);
end;
  exit(atanApprox(y / x) - PI);
end;
  if y > 0 then begin
  exit(PI / 2);
end;
  if y < 0 then begin
  exit(-PI / 2);
end;
  exit(0);
end;
function deg(rad: real): real;
begin
  exit((rad * 180) / PI);
end;
function zero_matrix(h: integer; w: integer): RealArrayArray;
var
  zero_matrix_out: array of RealArray;
  zero_matrix_i: integer;
  zero_matrix_row: array of real;
  zero_matrix_j: integer;
begin
  zero_matrix_out := [];
  zero_matrix_i := 0;
  while zero_matrix_i < h do begin
  zero_matrix_row := [];
  zero_matrix_j := 0;
  while zero_matrix_j < w do begin
  zero_matrix_row := concat(zero_matrix_row, [0]);
  zero_matrix_j := zero_matrix_j + 1;
end;
  zero_matrix_out := concat(zero_matrix_out, [zero_matrix_row]);
  zero_matrix_i := zero_matrix_i + 1;
end;
  exit(zero_matrix_out);
end;
function convolve(img: RealArrayArray; kernel: RealArrayArray): RealArrayArray;
var
  convolve_h: integer;
  convolve_w: integer;
  convolve_k: integer;
  convolve_pad: integer;
  convolve_out: RealArrayArray;
  convolve_y: integer;
  convolve_x: integer;
  convolve_sum: real;
  convolve_ky: integer;
  convolve_kx: integer;
  convolve_pixel: real;
  convolve_weight: real;
begin
  convolve_h := Length(img);
  convolve_w := Length(img[0]);
  convolve_k := Length(kernel);
  convolve_pad := convolve_k div 2;
  convolve_out := zero_matrix(convolve_h, convolve_w);
  convolve_y := convolve_pad;
  while convolve_y < (convolve_h - convolve_pad) do begin
  convolve_x := convolve_pad;
  while convolve_x < (convolve_w - convolve_pad) do begin
  convolve_sum := 0;
  convolve_ky := 0;
  while convolve_ky < convolve_k do begin
  convolve_kx := 0;
  while convolve_kx < convolve_k do begin
  convolve_pixel := img[(convolve_y - convolve_pad) + convolve_ky][(convolve_x - convolve_pad) + convolve_kx];
  convolve_weight := kernel[convolve_ky][convolve_kx];
  convolve_sum := convolve_sum + (convolve_pixel * convolve_weight);
  convolve_kx := convolve_kx + 1;
end;
  convolve_ky := convolve_ky + 1;
end;
  convolve_out[convolve_y][convolve_x] := convolve_sum;
  convolve_x := convolve_x + 1;
end;
  convolve_y := convolve_y + 1;
end;
  exit(convolve_out);
end;
function gaussian_blur(img: RealArrayArray): RealArrayArray;
begin
  exit(convolve(img, GAUSSIAN_KERNEL));
end;
function sobel_filter(img: RealArrayArray): specialize TFPGMap<string, Variant>;
var
  sobel_filter_gx: RealArrayArray;
  sobel_filter_gy: RealArrayArray;
  sobel_filter_h: integer;
  sobel_filter_w: integer;
  sobel_filter_grad: RealArrayArray;
  sobel_filter_dir: RealArrayArray;
  sobel_filter_i: integer;
  sobel_filter_j: integer;
  sobel_filter_gxx: real;
  sobel_filter_gyy: real;
begin
  sobel_filter_gx := convolve(img, SOBEL_GX);
  sobel_filter_gy := convolve(img, SOBEL_GY);
  sobel_filter_h := Length(img);
  sobel_filter_w := Length(img[0]);
  sobel_filter_grad := zero_matrix(sobel_filter_h, sobel_filter_w);
  sobel_filter_dir := zero_matrix(sobel_filter_h, sobel_filter_w);
  sobel_filter_i := 0;
  while sobel_filter_i < sobel_filter_h do begin
  sobel_filter_j := 0;
  while sobel_filter_j < sobel_filter_w do begin
  sobel_filter_gxx := sobel_filter_gx[sobel_filter_i][sobel_filter_j];
  sobel_filter_gyy := sobel_filter_gy[sobel_filter_i][sobel_filter_j];
  sobel_filter_grad[sobel_filter_i][sobel_filter_j] := sqrtApprox((sobel_filter_gxx * sobel_filter_gxx) + (sobel_filter_gyy * sobel_filter_gyy));
  sobel_filter_dir[sobel_filter_i][sobel_filter_j] := deg(atan2Approx(sobel_filter_gyy, sobel_filter_gxx)) + 180;
  sobel_filter_j := sobel_filter_j + 1;
end;
  sobel_filter_i := sobel_filter_i + 1;
end;
  exit(Map1(sobel_filter_dir, sobel_filter_grad));
end;
function suppress_non_maximum(h: integer; w: integer; direction: RealArrayArray; grad: RealArrayArray): RealArrayArray;
var
  suppress_non_maximum_dest: RealArrayArray;
  suppress_non_maximum_r: integer;
  suppress_non_maximum_c: integer;
  suppress_non_maximum_angle: real;
  suppress_non_maximum_q: real;
  suppress_non_maximum_p: real;
begin
  suppress_non_maximum_dest := zero_matrix(h, w);
  suppress_non_maximum_r := 1;
  while suppress_non_maximum_r < (h - 1) do begin
  suppress_non_maximum_c := 1;
  while suppress_non_maximum_c < (w - 1) do begin
  suppress_non_maximum_angle := direction[suppress_non_maximum_r][suppress_non_maximum_c];
  suppress_non_maximum_q := 0;
  suppress_non_maximum_p := 0;
  if (((suppress_non_maximum_angle >= 0) and (suppress_non_maximum_angle < 22.5)) or ((suppress_non_maximum_angle >= 157.5) and (suppress_non_maximum_angle <= 180))) or (suppress_non_maximum_angle >= 337.5) then begin
  suppress_non_maximum_q := grad[suppress_non_maximum_r][suppress_non_maximum_c + 1];
  suppress_non_maximum_p := grad[suppress_non_maximum_r][suppress_non_maximum_c - 1];
end else begin
  if ((suppress_non_maximum_angle >= 22.5) and (suppress_non_maximum_angle < 67.5)) or ((suppress_non_maximum_angle >= 202.5) and (suppress_non_maximum_angle < 247.5)) then begin
  suppress_non_maximum_q := grad[suppress_non_maximum_r + 1][suppress_non_maximum_c - 1];
  suppress_non_maximum_p := grad[suppress_non_maximum_r - 1][suppress_non_maximum_c + 1];
end else begin
  if ((suppress_non_maximum_angle >= 67.5) and (suppress_non_maximum_angle < 112.5)) or ((suppress_non_maximum_angle >= 247.5) and (suppress_non_maximum_angle < 292.5)) then begin
  suppress_non_maximum_q := grad[suppress_non_maximum_r + 1][suppress_non_maximum_c];
  suppress_non_maximum_p := grad[suppress_non_maximum_r - 1][suppress_non_maximum_c];
end else begin
  suppress_non_maximum_q := grad[suppress_non_maximum_r - 1][suppress_non_maximum_c - 1];
  suppress_non_maximum_p := grad[suppress_non_maximum_r + 1][suppress_non_maximum_c + 1];
end;
end;
end;
  if (grad[suppress_non_maximum_r][suppress_non_maximum_c] >= suppress_non_maximum_q) and (grad[suppress_non_maximum_r][suppress_non_maximum_c] >= suppress_non_maximum_p) then begin
  suppress_non_maximum_dest[suppress_non_maximum_r][suppress_non_maximum_c] := grad[suppress_non_maximum_r][suppress_non_maximum_c];
end;
  suppress_non_maximum_c := suppress_non_maximum_c + 1;
end;
  suppress_non_maximum_r := suppress_non_maximum_r + 1;
end;
  exit(suppress_non_maximum_dest);
end;
procedure double_threshold(h: integer; w: integer; img: RealArrayArray; low: real; high: real; weak: real; strong: real);
var
  double_threshold_r: integer;
  double_threshold_c: integer;
  double_threshold_v: real;
begin
  double_threshold_r := 0;
  while double_threshold_r < h do begin
  double_threshold_c := 0;
  while double_threshold_c < w do begin
  double_threshold_v := img[double_threshold_r][double_threshold_c];
  if double_threshold_v >= high then begin
  img[double_threshold_r][double_threshold_c] := strong;
end else begin
  if double_threshold_v < low then begin
  img[double_threshold_r][double_threshold_c] := 0;
end else begin
  img[double_threshold_r][double_threshold_c] := weak;
end;
end;
  double_threshold_c := double_threshold_c + 1;
end;
  double_threshold_r := double_threshold_r + 1;
end;
end;
procedure track_edge(h: integer; w: integer; img: RealArrayArray; weak: real; strong: real);
var
  track_edge_r: integer;
  track_edge_c: integer;
begin
  track_edge_r := 1;
  while track_edge_r < (h - 1) do begin
  track_edge_c := 1;
  while track_edge_c < (w - 1) do begin
  if img[track_edge_r][track_edge_c] = weak then begin
  if (((((((img[track_edge_r + 1][track_edge_c] = strong) or (img[track_edge_r - 1][track_edge_c] = strong)) or (img[track_edge_r][track_edge_c + 1] = strong)) or (img[track_edge_r][track_edge_c - 1] = strong)) or (img[track_edge_r - 1][track_edge_c - 1] = strong)) or (img[track_edge_r - 1][track_edge_c + 1] = strong)) or (img[track_edge_r + 1][track_edge_c - 1] = strong)) or (img[track_edge_r + 1][track_edge_c + 1] = strong) then begin
  img[track_edge_r][track_edge_c] := strong;
end else begin
  img[track_edge_r][track_edge_c] := 0;
end;
end;
  track_edge_c := track_edge_c + 1;
end;
  track_edge_r := track_edge_r + 1;
end;
end;
function canny(image: RealArrayArray; low: real; high: real; weak: real; strong: real): RealArrayArray;
var
  canny_blurred: RealArrayArray;
  canny_sob: specialize TFPGMap<string, Variant>;
  canny_grad: integer;
  canny_direction: integer;
  canny_h: integer;
  canny_w: integer;
  canny_suppressed: RealArrayArray;
begin
  canny_blurred := gaussian_blur(image);
  canny_sob := sobel_filter(canny_blurred);
  canny_grad_idx := canny_sob.IndexOf('grad');
  if canny_grad_idx <> -1 then begin
  canny_grad := canny_sob.Data[canny_grad_idx];
end else begin
  canny_grad := 0;
end;
  canny_direction_idx := canny_sob.IndexOf('dir');
  if canny_direction_idx <> -1 then begin
  canny_direction := canny_sob.Data[canny_direction_idx];
end else begin
  canny_direction := 0;
end;
  canny_h := Length(image);
  canny_w := Length(image[0]);
  canny_suppressed := suppress_non_maximum(canny_h, canny_w, canny_direction, canny_grad);
  double_threshold(canny_h, canny_w, canny_suppressed, low, high, weak, strong);
  track_edge(canny_h, canny_w, canny_suppressed, weak, strong);
  exit(canny_suppressed);
end;
procedure print_image(img: RealArrayArray);
var
  print_image_r: integer;
  print_image_c: integer;
  print_image_line: string;
begin
  print_image_r := 0;
  while print_image_r < Length(img) do begin
  print_image_c := 0;
  print_image_line := '';
  while print_image_c < Length(img[print_image_r]) do begin
  print_image_line := (print_image_line + IntToStr(Trunc(img[print_image_r][print_image_c]))) + ' ';
  print_image_c := print_image_c + 1;
end;
  writeln(print_image_line);
  print_image_r := print_image_r + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  GAUSSIAN_KERNEL := [[0.0625, 0.125, 0.0625], [0.125, 0.25, 0.125], [0.0625, 0.125, 0.0625]];
  SOBEL_GX := [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];
  SOBEL_GY := [[1, 2, 1], [0, 0, 0], [-1, -2, -1]];
  image := [[0, 0, 0, 0, 0], [0, 255, 255, 255, 0], [0, 255, 255, 255, 0], [0, 255, 255, 255, 0], [0, 0, 0, 0, 0]];
  edges := canny(image, 20, 40, 128, 255);
  print_image(edges);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
