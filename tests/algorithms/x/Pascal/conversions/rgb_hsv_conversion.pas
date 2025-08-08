{$mode objfpc}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  rgb: IntArray;
  hsv: RealArray;
  hsv2: RealArray;
  x: real;
  hsv1: RealArray;
  a: real;
  red: integer;
  c: real;
  value: real;
  b: real;
  hue: real;
  saturation: real;
  blue: integer;
  green: integer;
function absf(x: real): real; forward;
function fmod(a: real; b: real): real; forward;
function roundf(x: real): integer; forward;
function maxf(a: real; b: real; c: real): real; forward;
function minf(a: real; b: real; c: real): real; forward;
function hsv_to_rgb(hue: real; saturation: real; value: real): IntArray; forward;
function rgb_to_hsv(red: integer; green: integer; blue: integer): RealArray; forward;
function approximately_equal_hsv(hsv1: RealArray; hsv2: RealArray): boolean; forward;
function absf(x: real): real;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function fmod(a: real; b: real): real;
begin
  exit(a - (b * Trunc(a / b)));
end;
function roundf(x: real): integer;
begin
  if x >= 0 then begin
  exit(Trunc(x + 0.5));
end;
  exit(Trunc(x - 0.5));
end;
function maxf(a: real; b: real; c: real): real;
var
  maxf_m: real;
begin
  maxf_m := a;
  if b > maxf_m then begin
  maxf_m := b;
end;
  if c > maxf_m then begin
  maxf_m := c;
end;
  exit(maxf_m);
end;
function minf(a: real; b: real; c: real): real;
var
  minf_m: real;
begin
  minf_m := a;
  if b < minf_m then begin
  minf_m := b;
end;
  if c < minf_m then begin
  minf_m := c;
end;
  exit(minf_m);
end;
function hsv_to_rgb(hue: real; saturation: real; value: real): IntArray;
var
  hsv_to_rgb_chroma: real;
  hsv_to_rgb_hue_section: real;
  hsv_to_rgb_second_largest_component: real;
  hsv_to_rgb_match_value: real;
  hsv_to_rgb_red: integer;
  hsv_to_rgb_green: integer;
  hsv_to_rgb_blue: integer;
begin
  if (hue < 0) or (hue > 360) then begin
  writeln('hue should be between 0 and 360');
  exit([]);
end;
  if (saturation < 0) or (saturation > 1) then begin
  writeln('saturation should be between 0 and 1');
  exit([]);
end;
  if (value < 0) or (value > 1) then begin
  writeln('value should be between 0 and 1');
  exit([]);
end;
  hsv_to_rgb_chroma := value * saturation;
  hsv_to_rgb_hue_section := hue / 60;
  hsv_to_rgb_second_largest_component := hsv_to_rgb_chroma * (1 - absf(fmod(hsv_to_rgb_hue_section, 2) - 1));
  hsv_to_rgb_match_value := value - hsv_to_rgb_chroma;
  if (hsv_to_rgb_hue_section >= 0) and (hsv_to_rgb_hue_section <= 1) then begin
  hsv_to_rgb_red := roundf(255 * (hsv_to_rgb_chroma + hsv_to_rgb_match_value));
  hsv_to_rgb_green := roundf(255 * (hsv_to_rgb_second_largest_component + hsv_to_rgb_match_value));
  hsv_to_rgb_blue := roundf(255 * hsv_to_rgb_match_value);
end else begin
  if (hsv_to_rgb_hue_section > 1) and (hsv_to_rgb_hue_section <= 2) then begin
  hsv_to_rgb_red := roundf(255 * (hsv_to_rgb_second_largest_component + hsv_to_rgb_match_value));
  hsv_to_rgb_green := roundf(255 * (hsv_to_rgb_chroma + hsv_to_rgb_match_value));
  hsv_to_rgb_blue := roundf(255 * hsv_to_rgb_match_value);
end else begin
  if (hsv_to_rgb_hue_section > 2) and (hsv_to_rgb_hue_section <= 3) then begin
  hsv_to_rgb_red := roundf(255 * hsv_to_rgb_match_value);
  hsv_to_rgb_green := roundf(255 * (hsv_to_rgb_chroma + hsv_to_rgb_match_value));
  hsv_to_rgb_blue := roundf(255 * (hsv_to_rgb_second_largest_component + hsv_to_rgb_match_value));
end else begin
  if (hsv_to_rgb_hue_section > 3) and (hsv_to_rgb_hue_section <= 4) then begin
  hsv_to_rgb_red := roundf(255 * hsv_to_rgb_match_value);
  hsv_to_rgb_green := roundf(255 * (hsv_to_rgb_second_largest_component + hsv_to_rgb_match_value));
  hsv_to_rgb_blue := roundf(255 * (hsv_to_rgb_chroma + hsv_to_rgb_match_value));
end else begin
  if (hsv_to_rgb_hue_section > 4) and (hsv_to_rgb_hue_section <= 5) then begin
  hsv_to_rgb_red := roundf(255 * (hsv_to_rgb_second_largest_component + hsv_to_rgb_match_value));
  hsv_to_rgb_green := roundf(255 * hsv_to_rgb_match_value);
  hsv_to_rgb_blue := roundf(255 * (hsv_to_rgb_chroma + hsv_to_rgb_match_value));
end else begin
  hsv_to_rgb_red := roundf(255 * (hsv_to_rgb_chroma + hsv_to_rgb_match_value));
  hsv_to_rgb_green := roundf(255 * hsv_to_rgb_match_value);
  hsv_to_rgb_blue := roundf(255 * (hsv_to_rgb_second_largest_component + hsv_to_rgb_match_value));
end;
end;
end;
end;
end;
  exit([hsv_to_rgb_red, hsv_to_rgb_green, hsv_to_rgb_blue]);
end;
function rgb_to_hsv(red: integer; green: integer; blue: integer): RealArray;
var
  rgb_to_hsv_float_red: real;
  rgb_to_hsv_float_green: real;
  rgb_to_hsv_float_blue: real;
  rgb_to_hsv_value: real;
  rgb_to_hsv_min_val: real;
  rgb_to_hsv_chroma: real;
  rgb_to_hsv_saturation: real;
  rgb_to_hsv_hue: real;
begin
  if (red < 0) or (red > 255) then begin
  writeln('red should be between 0 and 255');
  exit([]);
end;
  if (green < 0) or (green > 255) then begin
  writeln('green should be between 0 and 255');
  exit([]);
end;
  if (blue < 0) or (blue > 255) then begin
  writeln('blue should be between 0 and 255');
  exit([]);
end;
  rgb_to_hsv_float_red := red / 255;
  rgb_to_hsv_float_green := green / 255;
  rgb_to_hsv_float_blue := blue / 255;
  rgb_to_hsv_value := maxf(rgb_to_hsv_float_red, rgb_to_hsv_float_green, rgb_to_hsv_float_blue);
  rgb_to_hsv_min_val := minf(rgb_to_hsv_float_red, rgb_to_hsv_float_green, rgb_to_hsv_float_blue);
  rgb_to_hsv_chroma := rgb_to_hsv_value - rgb_to_hsv_min_val;
  if rgb_to_hsv_value = 0 then begin
  rgb_to_hsv_saturation := 0;
end else begin
  rgb_to_hsv_saturation := rgb_to_hsv_chroma / rgb_to_hsv_value;
end;
  if rgb_to_hsv_chroma = 0 then begin
  rgb_to_hsv_hue := 0;
end else begin
  if rgb_to_hsv_value = rgb_to_hsv_float_red then begin
  rgb_to_hsv_hue := 60 * (0 + ((rgb_to_hsv_float_green - rgb_to_hsv_float_blue) / rgb_to_hsv_chroma));
end else begin
  if rgb_to_hsv_value = rgb_to_hsv_float_green then begin
  rgb_to_hsv_hue := 60 * (2 + ((rgb_to_hsv_float_blue - rgb_to_hsv_float_red) / rgb_to_hsv_chroma));
end else begin
  rgb_to_hsv_hue := 60 * (4 + ((rgb_to_hsv_float_red - rgb_to_hsv_float_green) / rgb_to_hsv_chroma));
end;
end;
end;
  rgb_to_hsv_hue := fmod(rgb_to_hsv_hue + 360, 360);
  exit([rgb_to_hsv_hue, rgb_to_hsv_saturation, rgb_to_hsv_value]);
end;
function approximately_equal_hsv(hsv1: RealArray; hsv2: RealArray): boolean;
var
  approximately_equal_hsv_check_hue: boolean;
  approximately_equal_hsv_check_saturation: boolean;
  approximately_equal_hsv_check_value: boolean;
begin
  approximately_equal_hsv_check_hue := absf(hsv1[0] - hsv2[0]) < 0.2;
  approximately_equal_hsv_check_saturation := absf(hsv1[1] - hsv2[1]) < 0.002;
  approximately_equal_hsv_check_value := absf(hsv1[2] - hsv2[2]) < 0.002;
  exit((approximately_equal_hsv_check_hue and approximately_equal_hsv_check_saturation) and approximately_equal_hsv_check_value);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  rgb := hsv_to_rgb(180, 0.5, 0.5);
  writeln(list_int_to_str(rgb));
  hsv := rgb_to_hsv(64, 128, 128);
  writeln(list_real_to_str(hsv));
  writeln(LowerCase(BoolToStr(approximately_equal_hsv(hsv, [180, 0.5, 0.5]), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
