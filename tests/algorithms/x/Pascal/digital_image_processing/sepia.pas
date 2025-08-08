{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
  image: array of IntArrayArray;
  sepia: IntArrayArrayArray;
  red: integer;
  blue: integer;
  factor: integer;
  value: integer;
  img: IntArrayArrayArray;
  green: integer;
function normalize(value: integer): integer; forward;
function to_grayscale(blue: integer; green: integer; red: integer): integer; forward;
function make_sepia(img: IntArrayArrayArray; factor: integer): IntArrayArrayArray; forward;
function normalize(value: integer): integer;
begin
  if value > 255 then begin
  exit(255);
end;
  exit(value);
end;
function to_grayscale(blue: integer; green: integer; red: integer): integer;
var
  to_grayscale_gs: real;
begin
  to_grayscale_gs := ((0.2126 * Double(red)) + (0.587 * Double(green))) + (0.114 * Double(blue));
  exit(Trunc(to_grayscale_gs));
end;
function make_sepia(img: IntArrayArrayArray; factor: integer): IntArrayArrayArray;
var
  make_sepia_pixel_h: integer;
  make_sepia_pixel_v: integer;
  make_sepia_i: integer;
  make_sepia_j: integer;
  make_sepia_pixel: array of integer;
  make_sepia_grey: integer;
begin
  make_sepia_pixel_h := Length(img);
  make_sepia_pixel_v := Length(img[0]);
  make_sepia_i := 0;
  while make_sepia_i < make_sepia_pixel_h do begin
  make_sepia_j := 0;
  while make_sepia_j < make_sepia_pixel_v do begin
  make_sepia_pixel := img[make_sepia_i][make_sepia_j];
  make_sepia_grey := to_grayscale(make_sepia_pixel[0], make_sepia_pixel[1], make_sepia_pixel[2]);
  img[make_sepia_i][make_sepia_j] := [normalize(make_sepia_grey), normalize(make_sepia_grey + factor), normalize(make_sepia_grey + (2 * factor))];
  make_sepia_j := make_sepia_j + 1;
end;
  make_sepia_i := make_sepia_i + 1;
end;
  exit(img);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  image := [[[10, 20, 30], [40, 50, 60]], [[70, 80, 90], [200, 150, 100]]];
  sepia := make_sepia(image, 20);
  writeln(list_list_int_to_str(sepia));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
