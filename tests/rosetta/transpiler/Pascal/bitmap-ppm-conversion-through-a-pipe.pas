{$mode objfpc}
program Main;
uses SysUtils;
type Pixel = record
  R: integer;
  G: integer;
  B: integer;
end;
type PixelArray = array of Pixel;
type PixelArrayArray = array of PixelArray;
type Bitmap = record
  cols: integer;
  rows: integer;
  px: array of PixelArray;
end;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  y: integer;
  p: Pixel;
  x: integer;
  seed: integer;
function makeBitmap(cols: integer; rows: integer; px: PixelArrayArray): Bitmap; forward;
function makePixel(R: integer; G: integer; B: integer): Pixel; forward;
function pixelFromRgb(c: integer): Pixel; forward;
function rgbFromPixel(p: Pixel): integer; forward;
function NewBitmap(x: integer; y: integer): Bitmap; forward;
procedure FillRgb(b: Bitmap; c: integer); forward;
function SetPxRgb(b: Bitmap; x: integer; y: integer; c: integer): boolean; forward;
function nextRand(seed: integer): integer; forward;
procedure main(); forward;
function makeBitmap(cols: integer; rows: integer; px: PixelArrayArray): Bitmap;
begin
  Result.cols := cols;
  Result.rows := rows;
  Result.px := px;
end;
function makePixel(R: integer; G: integer; B: integer): Pixel;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;
function pixelFromRgb(c: integer): Pixel;
var
  pixelFromRgb_r: integer;
  pixelFromRgb_g: integer;
  pixelFromRgb_b: integer;
begin
  pixelFromRgb_r := Trunc(c div 65536) mod 256;
  pixelFromRgb_g := Trunc(c div 256) mod 256;
  pixelFromRgb_b := c mod 256;
  exit(makePixel(pixelFromRgb_r, pixelFromRgb_g, pixelFromRgb_b));
end;
function rgbFromPixel(p: Pixel): integer;
begin
  exit(((p.R * 65536) + (p.G * 256)) + p.B);
end;
function NewBitmap(x: integer; y: integer): Bitmap;
var
  NewBitmap_data: array of PixelArray;
  NewBitmap_row: integer;
  NewBitmap_r: array of Pixel;
  NewBitmap_col: integer;
begin
  NewBitmap_data := [];
  NewBitmap_row := 0;
  while NewBitmap_row < y do begin
  NewBitmap_r := [];
  NewBitmap_col := 0;
  while NewBitmap_col < x do begin
  NewBitmap_r := concat(NewBitmap_r, [makePixel(0, 0, 0)]);
  NewBitmap_col := NewBitmap_col + 1;
end;
  NewBitmap_data := concat(NewBitmap_data, [NewBitmap_r]);
  NewBitmap_row := NewBitmap_row + 1;
end;
  exit(makeBitmap(x, y, NewBitmap_data));
end;
procedure FillRgb(b: Bitmap; c: integer);
var
  FillRgb_px: array of PixelArray;
  FillRgb_row: array of Pixel;
begin
  y := 0;
  p := pixelFromRgb(c);
  while y < b.rows do begin
  x := 0;
  while x < b.cols do begin
  FillRgb_px := b.px;
  FillRgb_row := FillRgb_px[y];
  FillRgb_row[x] := p;
  FillRgb_px[y] := FillRgb_row;
  b.px := FillRgb_px;
  x := x + 1;
end;
  y := y + 1;
end;
end;
function SetPxRgb(b: Bitmap; x: integer; y: integer; c: integer): boolean;
var
  SetPxRgb_px: array of PixelArray;
  SetPxRgb_row: array of Pixel;
begin
  if (((x < 0) or (x >= b.cols)) or (y < 0)) or (y >= b.rows) then begin
  exit(false);
end;
  SetPxRgb_px := b.px;
  SetPxRgb_row := SetPxRgb_px[y];
  SetPxRgb_row[x] := pixelFromRgb(c);
  SetPxRgb_px[y] := SetPxRgb_row;
  b.px := SetPxRgb_px;
  exit(true);
end;
function nextRand(seed: integer): integer;
begin
  exit(((seed * 1664525) + 1013904223) mod 2147483648);
end;
procedure main();
var
  main_bm: Bitmap;
  main_i: integer;
begin
  main_bm := NewBitmap(400, 300);
  FillRgb(main_bm, 12615744);
  seed := _now();
  main_i := 0;
  while main_i < 2000 do begin
  seed := nextRand(seed);
  x := seed mod 400;
  seed := nextRand(seed);
  y := seed mod 300;
  SetPxRgb(main_bm, x, y, 8405024);
  main_i := main_i + 1;
end;
  x := 0;
  while x < 400 do begin
  y := 240;
  while y < 245 do begin
  SetPxRgb(main_bm, x, y, 8405024);
  y := y + 1;
end;
  y := 260;
  while y < 265 do begin
  SetPxRgb(main_bm, x, y, 8405024);
  y := y + 1;
end;
  x := x + 1;
end;
  y := 0;
  while y < 300 do begin
  x := 80;
  while x < 85 do begin
  SetPxRgb(main_bm, x, y, 8405024);
  x := x + 1;
end;
  x := 95;
  while x < 100 do begin
  SetPxRgb(main_bm, x, y, 8405024);
  x := x + 1;
end;
  y := y + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
