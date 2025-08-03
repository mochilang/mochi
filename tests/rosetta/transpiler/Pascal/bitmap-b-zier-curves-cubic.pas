{$mode objfpc}
program Main;
uses SysUtils, fgl;
type Pixel = record
  r: integer;
  g: integer;
  b: integer;
end;
type PixelArray = array of Pixel;
type PixelArrayArray = array of PixelArray;
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
  b3Seg: integer;
  cols: integer;
  rows: integer;
  y: integer;
  x: integer;
  x0: integer;
  y0: integer;
  b: specialize TFPGMap<string, Variant>;
function Map1(cols: integer; newBitmap_d: PixelArrayArray; rows: integer): specialize TFPGMap<string, Variant>; forward;
function makePixel(r: integer; g: integer; b: integer): Pixel; forward;
function pixelFromRgb(rgb: integer): Pixel; forward;
function newBitmap(cols: integer; rows: integer): specialize TFPGMap<string, Variant>; forward;
procedure setPx(b: specialize TFPGMap<string, Variant>; x: integer; y: integer; p: Pixel); forward;
procedure fill(b: specialize TFPGMap<string, Variant>; p: Pixel); forward;
procedure fillRgb(b: specialize TFPGMap<string, Variant>; rgb: integer); forward;
procedure line(b: specialize TFPGMap<string, Variant>; x0: integer; y0: integer; x1: integer; y1: integer; p: Pixel); forward;
procedure bezier3(b: specialize TFPGMap<string, Variant>; x1: integer; y1: integer; x2: integer; y2: integer; x3: integer; y3: integer; x4: integer; y4: integer; p: Pixel); forward;
function Map1(cols: integer; newBitmap_d: PixelArrayArray; rows: integer): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('cols', Variant(cols));
  Result.AddOrSetData('rows', Variant(rows));
  Result.AddOrSetData('data', Variant(newBitmap_d));
end;
function makePixel(r: integer; g: integer; b: integer): Pixel;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
end;
function pixelFromRgb(rgb: integer): Pixel;
var
  pixelFromRgb_r: integer;
  pixelFromRgb_g: integer;
  pixelFromRgb_b: integer;
begin
  pixelFromRgb_r := Trunc((rgb div 65536) mod 256);
  pixelFromRgb_g := Trunc((rgb div 256) mod 256);
  pixelFromRgb_b := Trunc(rgb mod 256);
  exit(makePixel(pixelFromRgb_r, pixelFromRgb_g, pixelFromRgb_b));
end;
function newBitmap(cols: integer; rows: integer): specialize TFPGMap<string, Variant>;
var
  newBitmap_d: array of PixelArray;
  newBitmap_y: integer;
  newBitmap_row: array of Pixel;
  newBitmap_x: integer;
begin
  newBitmap_d := [];
  newBitmap_y := 0;
  while newBitmap_y < rows do begin
  newBitmap_row := [];
  newBitmap_x := 0;
  while newBitmap_x < cols do begin
  newBitmap_row := concat(newBitmap_row, [makePixel(0, 0, 0)]);
  newBitmap_x := newBitmap_x + 1;
end;
  newBitmap_d := concat(newBitmap_d, [newBitmap_row]);
  newBitmap_y := newBitmap_y + 1;
end;
  exit(Map1(cols, newBitmap_d, rows));
end;
procedure setPx(b: specialize TFPGMap<string, Variant>; x: integer; y: integer; p: Pixel);
begin
  cols := Trunc(b['cols']);
  rows := Trunc(b['rows']);
  if (((x >= 0) and (x < cols)) and (y >= 0)) and (y < rows) then begin
  PixelArrayArray(b['data'])[y][x] := p;
end;
end;
procedure fill(b: specialize TFPGMap<string, Variant>; p: Pixel);
begin
  cols := Trunc(b['cols']);
  rows := Trunc(b['rows']);
  y := 0;
  while y < rows do begin
  x := 0;
  while x < cols do begin
  PixelArrayArray(b['data'])[y][x] := p;
  x := x + 1;
end;
  y := y + 1;
end;
end;
procedure fillRgb(b: specialize TFPGMap<string, Variant>; rgb: integer);
begin
  fill(b, pixelFromRgb(rgb));
end;
procedure line(b: specialize TFPGMap<string, Variant>; x0: integer; y0: integer; x1: integer; y1: integer; p: Pixel);
var
  line_dx: integer;
  line_dy: integer;
  line_sx: integer;
  line_sy: integer;
  line_err: integer;
  line_e2: integer;
begin
  line_dx := x1 - x0;
  if line_dx < 0 then begin
  line_dx := -line_dx;
end;
  line_dy := y1 - y0;
  if line_dy < 0 then begin
  line_dy := -line_dy;
end;
  line_sx := -1;
  if x0 < x1 then begin
  line_sx := 1;
end;
  line_sy := -1;
  if y0 < y1 then begin
  line_sy := 1;
end;
  line_err := line_dx - line_dy;
  while true do begin
  setPx(b, x0, y0, p);
  if (x0 = x1) and (y0 = y1) then begin
  break;
end;
  line_e2 := 2 * line_err;
  if line_e2 > (0 - line_dy) then begin
  line_err := line_err - line_dy;
  x0 := x0 + line_sx;
end;
  if line_e2 < line_dx then begin
  line_err := line_err + line_dx;
  y0 := y0 + line_sy;
end;
end;
end;
procedure bezier3(b: specialize TFPGMap<string, Variant>; x1: integer; y1: integer; x2: integer; y2: integer; x3: integer; y3: integer; x4: integer; y4: integer; p: Pixel);
var
  bezier3_px: array of integer;
  bezier3_py: array of integer;
  bezier3_i: integer;
  bezier3_fx1: real;
  bezier3_fy1: real;
  bezier3_fx2: real;
  bezier3_fy2: real;
  bezier3_fx3: real;
  bezier3_fy3: real;
  bezier3_fx4: real;
  bezier3_fy4: real;
  bezier3_d: real;
  bezier3_a: real;
  bezier3_bcoef: real;
  bezier3_ccoef: real;
  bezier3_a2: real;
  bezier3_b2: real;
  bezier3_c2: real;
  bezier3_d2: real;
begin
  bezier3_px := [];
  bezier3_py := [];
  bezier3_i := 0;
  while bezier3_i <= b3Seg do begin
  bezier3_px := concat(bezier3_px, [0]);
  bezier3_py := concat(bezier3_py, [0]);
  bezier3_i := bezier3_i + 1;
end;
  bezier3_fx1 := Double(x1);
  bezier3_fy1 := Double(y1);
  bezier3_fx2 := Double(x2);
  bezier3_fy2 := Double(y2);
  bezier3_fx3 := Double(x3);
  bezier3_fy3 := Double(y3);
  bezier3_fx4 := Double(x4);
  bezier3_fy4 := Double(y4);
  bezier3_i := 0;
  while bezier3_i <= b3Seg do begin
  bezier3_d := Double(bezier3_i) / Double(b3Seg);
  bezier3_a := 1 - bezier3_d;
  bezier3_bcoef := bezier3_a * bezier3_a;
  bezier3_ccoef := bezier3_d * bezier3_d;
  bezier3_a2 := bezier3_a * bezier3_bcoef;
  bezier3_b2 := (3 * bezier3_bcoef) * bezier3_d;
  bezier3_c2 := (3 * bezier3_a) * bezier3_ccoef;
  bezier3_d2 := bezier3_ccoef * bezier3_d;
  bezier3_px[bezier3_i] := Trunc((((bezier3_a2 * bezier3_fx1) + (bezier3_b2 * bezier3_fx2)) + (bezier3_c2 * bezier3_fx3)) + (bezier3_d2 * bezier3_fx4));
  bezier3_py[bezier3_i] := Trunc((((bezier3_a2 * bezier3_fy1) + (bezier3_b2 * bezier3_fy2)) + (bezier3_c2 * bezier3_fy3)) + (bezier3_d2 * bezier3_fy4));
  bezier3_i := bezier3_i + 1;
end;
  x0 := bezier3_px[0];
  y0 := bezier3_py[0];
  bezier3_i := 1;
  while bezier3_i <= b3Seg do begin
  x := bezier3_px[bezier3_i];
  y := bezier3_py[bezier3_i];
  line(b, x0, y0, x, y, p);
  x0 := x;
  y0 := y;
  bezier3_i := bezier3_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  b3Seg := 30;
  b := newBitmap(400, 300);
  fillRgb(b, 16773055);
  bezier3(b, 20, 200, 700, 50, -300, 50, 380, 150, pixelFromRgb(4165615));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
