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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  img: array of IntArray;
  gaussian3: IntArrayArray;
  gaussian5: IntArrayArray;
  k_size: integer;
  x: real;
  sigma: real;
  image: IntArrayArray;
function expApprox(x: real): real; forward;
function gen_gaussian_kernel(k_size: integer; sigma: real): RealArrayArray; forward;
function gaussian_filter(image: IntArrayArray; k_size: integer; sigma: real): IntArrayArray; forward;
procedure print_image(image: IntArrayArray); forward;
function expApprox(x: real): real;
var
  expApprox_sum: real;
  expApprox_term: real;
  expApprox_n: integer;
begin
  expApprox_sum := 1;
  expApprox_term := 1;
  expApprox_n := 1;
  while expApprox_n < 10 do begin
  expApprox_term := (expApprox_term * x) / Double(expApprox_n);
  expApprox_sum := expApprox_sum + expApprox_term;
  expApprox_n := expApprox_n + 1;
end;
  exit(expApprox_sum);
end;
function gen_gaussian_kernel(k_size: integer; sigma: real): RealArrayArray;
var
  gen_gaussian_kernel_center: integer;
  gen_gaussian_kernel_kernel: array of RealArray;
  gen_gaussian_kernel_i: integer;
  gen_gaussian_kernel_row: array of real;
  gen_gaussian_kernel_j: integer;
  gen_gaussian_kernel_x: real;
  gen_gaussian_kernel_y: real;
  gen_gaussian_kernel_exponent: real;
  gen_gaussian_kernel_value: real;
begin
  gen_gaussian_kernel_center := k_size div 2;
  gen_gaussian_kernel_kernel := [];
  gen_gaussian_kernel_i := 0;
  while gen_gaussian_kernel_i < k_size do begin
  gen_gaussian_kernel_row := [];
  gen_gaussian_kernel_j := 0;
  while gen_gaussian_kernel_j < k_size do begin
  gen_gaussian_kernel_x := Double(gen_gaussian_kernel_i - gen_gaussian_kernel_center);
  gen_gaussian_kernel_y := Double(gen_gaussian_kernel_j - gen_gaussian_kernel_center);
  gen_gaussian_kernel_exponent := -(((gen_gaussian_kernel_x * gen_gaussian_kernel_x) + (gen_gaussian_kernel_y * gen_gaussian_kernel_y)) / ((2 * sigma) * sigma));
  gen_gaussian_kernel_value := (1 / ((2 * PI) * sigma)) * expApprox(gen_gaussian_kernel_exponent);
  gen_gaussian_kernel_row := concat(gen_gaussian_kernel_row, [gen_gaussian_kernel_value]);
  gen_gaussian_kernel_j := gen_gaussian_kernel_j + 1;
end;
  gen_gaussian_kernel_kernel := concat(gen_gaussian_kernel_kernel, [gen_gaussian_kernel_row]);
  gen_gaussian_kernel_i := gen_gaussian_kernel_i + 1;
end;
  exit(gen_gaussian_kernel_kernel);
end;
function gaussian_filter(image: IntArrayArray; k_size: integer; sigma: real): IntArrayArray;
var
  gaussian_filter_height: integer;
  gaussian_filter_width: integer;
  gaussian_filter_dst_height: integer;
  gaussian_filter_dst_width: integer;
  gaussian_filter_kernel: RealArrayArray;
  gaussian_filter_dst: array of IntArray;
  gaussian_filter_i: integer;
  gaussian_filter_row: array of integer;
  gaussian_filter_j: integer;
  gaussian_filter_sum: real;
  gaussian_filter_ki: integer;
  gaussian_filter_kj: integer;
begin
  gaussian_filter_height := Length(image);
  gaussian_filter_width := Length(image[0]);
  gaussian_filter_dst_height := (gaussian_filter_height - k_size) + 1;
  gaussian_filter_dst_width := (gaussian_filter_width - k_size) + 1;
  gaussian_filter_kernel := gen_gaussian_kernel(k_size, sigma);
  gaussian_filter_dst := [];
  gaussian_filter_i := 0;
  while gaussian_filter_i < gaussian_filter_dst_height do begin
  gaussian_filter_row := [];
  gaussian_filter_j := 0;
  while gaussian_filter_j < gaussian_filter_dst_width do begin
  gaussian_filter_sum := 0;
  gaussian_filter_ki := 0;
  while gaussian_filter_ki < k_size do begin
  gaussian_filter_kj := 0;
  while gaussian_filter_kj < k_size do begin
  gaussian_filter_sum := gaussian_filter_sum + (Double(image[gaussian_filter_i + gaussian_filter_ki][gaussian_filter_j + gaussian_filter_kj]) * gaussian_filter_kernel[gaussian_filter_ki][gaussian_filter_kj]);
  gaussian_filter_kj := gaussian_filter_kj + 1;
end;
  gaussian_filter_ki := gaussian_filter_ki + 1;
end;
  gaussian_filter_row := concat(gaussian_filter_row, IntArray([Trunc(gaussian_filter_sum)]));
  gaussian_filter_j := gaussian_filter_j + 1;
end;
  gaussian_filter_dst := concat(gaussian_filter_dst, [gaussian_filter_row]);
  gaussian_filter_i := gaussian_filter_i + 1;
end;
  exit(gaussian_filter_dst);
end;
procedure print_image(image: IntArrayArray);
var
  print_image_i: integer;
begin
  print_image_i := 0;
  while print_image_i < Length(image) do begin
  show_list(image[print_image_i]);
  print_image_i := print_image_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  img := [[52, 55, 61, 59, 79], [62, 59, 55, 104, 94], [63, 65, 66, 113, 144], [68, 70, 70, 126, 154], [70, 72, 69, 128, 155]];
  gaussian3 := gaussian_filter(img, 3, 1);
  gaussian5 := gaussian_filter(img, 5, 0.8);
  print_image(gaussian3);
  print_image(gaussian5);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
