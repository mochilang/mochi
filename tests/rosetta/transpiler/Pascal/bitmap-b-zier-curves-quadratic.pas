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
  b2Seg: integer;
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
procedure bezier2(b: specialize TFPGMap<string, Variant>; x1: integer; y1: integer; x2: integer; y2: integer; x3: integer; y3: integer; p: Pixel); forward;
function Map1(cols: integer; newBitmap_d: PixelArrayArray; rows: integer): specialize TFPGMap<string, Variant>;
var
  _ptr2: ^PixelArrayArray;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('cols', Variant(cols));
  Result.AddOrSetData('rows', Variant(rows));
  New(_ptr2);
  _ptr2^ := newBitmap_d;
  Result.AddOrSetData('data', Variant(PtrUInt(_ptr2)));
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
  PixelArrayArray(pointer(PtrUInt(b['data']))^)[y][x] := p;
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
  PixelArrayArray(pointer(PtrUInt(b['data']))^)[y][x] := p;
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
procedure bezier2(b: specialize TFPGMap<string, Variant>; x1: integer; y1: integer; x2: integer; y2: integer; x3: integer; y3: integer; p: Pixel);
var
  bezier2_px: array of integer;
  bezier2_py: array of integer;
  bezier2_i: integer;
  bezier2_fx1: real;
  bezier2_fy1: real;
  bezier2_fx2: real;
  bezier2_fy2: real;
  bezier2_fx3: real;
  bezier2_fy3: real;
  bezier2_c: real;
  bezier2_a: real;
  bezier2_a2: real;
  bezier2_b2: real;
  bezier2_c2: real;
begin
  bezier2_px := [];
  bezier2_py := [];
  bezier2_i := 0;
  while bezier2_i <= b2Seg do begin
  bezier2_px := concat(bezier2_px, [0]);
  bezier2_py := concat(bezier2_py, [0]);
  bezier2_i := bezier2_i + 1;
end;
  bezier2_fx1 := Double(x1);
  bezier2_fy1 := Double(y1);
  bezier2_fx2 := Double(x2);
  bezier2_fy2 := Double(y2);
  bezier2_fx3 := Double(x3);
  bezier2_fy3 := Double(y3);
  bezier2_i := 0;
  while bezier2_i <= b2Seg do begin
  bezier2_c := Double(bezier2_i) / Double(b2Seg);
  bezier2_a := 1 - bezier2_c;
  bezier2_a2 := bezier2_a * bezier2_a;
  bezier2_b2 := (2 * bezier2_c) * bezier2_a;
  bezier2_c2 := bezier2_c * bezier2_c;
  bezier2_px[bezier2_i] := Trunc(((bezier2_a2 * bezier2_fx1) + (bezier2_b2 * bezier2_fx2)) + (bezier2_c2 * bezier2_fx3));
  bezier2_py[bezier2_i] := Trunc(((bezier2_a2 * bezier2_fy1) + (bezier2_b2 * bezier2_fy2)) + (bezier2_c2 * bezier2_fy3));
  bezier2_i := bezier2_i + 1;
end;
  x0 := bezier2_px[0];
  y0 := bezier2_py[0];
  bezier2_i := 1;
  while bezier2_i <= b2Seg do begin
  x := bezier2_px[bezier2_i];
  y := bezier2_py[bezier2_i];
  line(b, x0, y0, x, y, p);
  x0 := x;
  y0 := y;
  bezier2_i := bezier2_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  b2Seg := 20;
  b := newBitmap(400, 300);
  fillRgb(b, 14614575);
  bezier2(b, 20, 150, 500, -100, 300, 280, pixelFromRgb(4165615));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
