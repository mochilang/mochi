{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
type IntArray = array of integer;
type BoolArray = array of boolean;
type IntArrayArray = array of IntArray;
type BoolArrayArray = array of BoolArray;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  rgb_img: array of array of array of integer;
  gray_img: array of array of real;
  img1: array of array of boolean;
  kernel1: array of array of integer;
  img2: array of array of boolean;
  kernel2: array of array of integer;
  gray: RealArrayArray;
  rgb: IntArrayArrayArray;
  kernel: IntArrayArray;
  image: BoolArrayArray;
function rgb_to_gray(rgb: IntArrayArrayArray): RealArrayArray; forward;
function gray_to_binary(gray: RealArrayArray): BoolArrayArray; forward;
function erosion(image: BoolArrayArray; kernel: IntArrayArray): BoolArrayArray; forward;
function rgb_to_gray(rgb: IntArrayArrayArray): RealArrayArray;
var
  rgb_to_gray_gray: array of RealArray;
  rgb_to_gray_i: integer;
  rgb_to_gray_row: array of real;
  rgb_to_gray_j: integer;
  rgb_to_gray_r: real;
  rgb_to_gray_g: real;
  rgb_to_gray_b: real;
  rgb_to_gray_value: real;
begin
  rgb_to_gray_gray := [];
  rgb_to_gray_i := 0;
  while rgb_to_gray_i < Length(rgb) do begin
  rgb_to_gray_row := [];
  rgb_to_gray_j := 0;
  while rgb_to_gray_j < Length(rgb[rgb_to_gray_i]) do begin
  rgb_to_gray_r := Double(rgb[rgb_to_gray_i][rgb_to_gray_j][0]);
  rgb_to_gray_g := Double(rgb[rgb_to_gray_i][rgb_to_gray_j][1]);
  rgb_to_gray_b := Double(rgb[rgb_to_gray_i][rgb_to_gray_j][2]);
  rgb_to_gray_value := ((0.2989 * rgb_to_gray_r) + (0.587 * rgb_to_gray_g)) + (0.114 * rgb_to_gray_b);
  rgb_to_gray_row := concat(rgb_to_gray_row, [rgb_to_gray_value]);
  rgb_to_gray_j := rgb_to_gray_j + 1;
end;
  rgb_to_gray_gray := concat(rgb_to_gray_gray, [rgb_to_gray_row]);
  rgb_to_gray_i := rgb_to_gray_i + 1;
end;
  exit(rgb_to_gray_gray);
end;
function gray_to_binary(gray: RealArrayArray): BoolArrayArray;
var
  gray_to_binary_binary: array of BoolArray;
  gray_to_binary_i: integer;
  gray_to_binary_row: array of boolean;
  gray_to_binary_j: integer;
begin
  gray_to_binary_binary := [];
  gray_to_binary_i := 0;
  while gray_to_binary_i < Length(gray) do begin
  gray_to_binary_row := [];
  gray_to_binary_j := 0;
  while gray_to_binary_j < Length(gray[gray_to_binary_i]) do begin
  gray_to_binary_row := concat(gray_to_binary_row, [(gray[gray_to_binary_i][gray_to_binary_j] > 127) and (gray[gray_to_binary_i][gray_to_binary_j] <= 255)]);
  gray_to_binary_j := gray_to_binary_j + 1;
end;
  gray_to_binary_binary := concat(gray_to_binary_binary, [gray_to_binary_row]);
  gray_to_binary_i := gray_to_binary_i + 1;
end;
  exit(gray_to_binary_binary);
end;
function erosion(image: BoolArrayArray; kernel: IntArrayArray): BoolArrayArray;
var
  erosion_h: integer;
  erosion_w: integer;
  erosion_k_h: integer;
  erosion_k_w: integer;
  erosion_pad_y: integer;
  erosion_pad_x: integer;
  erosion_padded: array of BoolArray;
  erosion_y: integer;
  erosion_row: array of boolean;
  erosion_x: integer;
  erosion_output: array of BoolArray;
  erosion_row_out: array of boolean;
  erosion_sum: integer;
  erosion_ky: integer;
  erosion_kx: integer;
begin
  erosion_h := Length(image);
  erosion_w := Length(image[0]);
  erosion_k_h := Length(kernel);
  erosion_k_w := Length(kernel[0]);
  erosion_pad_y := erosion_k_h div 2;
  erosion_pad_x := erosion_k_w div 2;
  erosion_padded := [];
  erosion_y := 0;
  while erosion_y < (erosion_h + (2 * erosion_pad_y)) do begin
  erosion_row := [];
  erosion_x := 0;
  while erosion_x < (erosion_w + (2 * erosion_pad_x)) do begin
  erosion_row := concat(erosion_row, [false]);
  erosion_x := erosion_x + 1;
end;
  erosion_padded := concat(erosion_padded, [erosion_row]);
  erosion_y := erosion_y + 1;
end;
  erosion_y := 0;
  while erosion_y < erosion_h do begin
  erosion_x := 0;
  while erosion_x < erosion_w do begin
  erosion_padded[erosion_pad_y + erosion_y][erosion_pad_x + erosion_x] := image[erosion_y][erosion_x];
  erosion_x := erosion_x + 1;
end;
  erosion_y := erosion_y + 1;
end;
  erosion_output := [];
  erosion_y := 0;
  while erosion_y < erosion_h do begin
  erosion_row_out := [];
  erosion_x := 0;
  while erosion_x < erosion_w do begin
  erosion_sum := 0;
  erosion_ky := 0;
  while erosion_ky < erosion_k_h do begin
  erosion_kx := 0;
  while erosion_kx < erosion_k_w do begin
  if (kernel[erosion_ky][erosion_kx] = 1) and erosion_padded[erosion_y + erosion_ky][erosion_x + erosion_kx] then begin
  erosion_sum := erosion_sum + 1;
end;
  erosion_kx := erosion_kx + 1;
end;
  erosion_ky := erosion_ky + 1;
end;
  erosion_row_out := concat(erosion_row_out, [erosion_sum = 5]);
  erosion_x := erosion_x + 1;
end;
  erosion_output := concat(erosion_output, [erosion_row_out]);
  erosion_y := erosion_y + 1;
end;
  exit(erosion_output);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  rgb_img := [[[127, 255, 0]]];
  writeln(list_int_to_str(rgb_to_gray(rgb_img)));
  gray_img := [[127, 255, 0]];
  writeln(list_int_to_str(gray_to_binary(gray_img)));
  img1 := [[true, true, false]];
  kernel1 := [[0, 1, 0]];
  writeln(list_int_to_str(erosion(img1, kernel1)));
  img2 := [[true, false, false]];
  kernel2 := [[1, 1, 0]];
  writeln(list_int_to_str(erosion(img2, kernel2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
