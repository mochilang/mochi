{$mode objfpc}
program Main;
uses SysUtils;
type Colour = record
  R: integer;
  G: integer;
  B: integer;
end;
type ColourArray = array of Colour;
type ColourArrayArray = array of ColourArray;
type Bitmap = record
  width: integer;
  height: integer;
  pixels: array of ColourArray;
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
  x: integer;
function makeBitmap(width: integer; height: integer; pixels: ColourArrayArray): Bitmap; forward;
function makeColour(R: integer; G: integer; B: integer): Colour; forward;
function newBitmap(w: integer; h: integer; c: Colour): Bitmap; forward;
procedure setPixel(b: Bitmap; x: integer; y: integer; c: Colour); forward;
procedure fillRect(b: Bitmap; x: integer; y: integer; w: integer; h: integer; c: Colour); forward;
function pad(n: integer; width: integer): string; forward;
function writePPMP3(b: Bitmap): string; forward;
procedure main(); forward;
function makeBitmap(width: integer; height: integer; pixels: ColourArrayArray): Bitmap;
begin
  Result.width := width;
  Result.height := height;
  Result.pixels := pixels;
end;
function makeColour(R: integer; G: integer; B: integer): Colour;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
end;
function newBitmap(w: integer; h: integer; c: Colour): Bitmap;
var
  newBitmap_rows: array of ColourArray;
  newBitmap_y: integer;
  newBitmap_row: array of Colour;
  newBitmap_x: integer;
begin
  newBitmap_rows := [];
  newBitmap_y := 0;
  while newBitmap_y < h do begin
  newBitmap_row := [];
  newBitmap_x := 0;
  while newBitmap_x < w do begin
  newBitmap_row := concat(newBitmap_row, [c]);
  newBitmap_x := newBitmap_x + 1;
end;
  newBitmap_rows := concat(newBitmap_rows, [newBitmap_row]);
  newBitmap_y := newBitmap_y + 1;
end;
  exit(makeBitmap(w, h, newBitmap_rows));
end;
procedure setPixel(b: Bitmap; x: integer; y: integer; c: Colour);
var
  setPixel_rows: array of ColourArray;
  setPixel_row: array of Colour;
begin
  setPixel_rows := b.pixels;
  setPixel_row := setPixel_rows[y];
  setPixel_row[x] := c;
  setPixel_rows[y] := setPixel_row;
  b.pixels := setPixel_rows;
end;
procedure fillRect(b: Bitmap; x: integer; y: integer; w: integer; h: integer; c: Colour);
var
  fillRect_yy: integer;
  fillRect_xx: integer;
begin
  fillRect_yy := y;
  while fillRect_yy < (y + h) do begin
  fillRect_xx := x;
  while fillRect_xx < (x + w) do begin
  setPixel(b, fillRect_xx, fillRect_yy, c);
  fillRect_xx := fillRect_xx + 1;
end;
  fillRect_yy := fillRect_yy + 1;
end;
end;
function pad(n: integer; width: integer): string;
var
  pad_s: string;
begin
  pad_s := IntToStr(n);
  while Length(pad_s) < width do begin
  pad_s := ' ' + pad_s;
end;
  exit(pad_s);
end;
function writePPMP3(b: Bitmap): string;
var
  writePPMP3_maxv: integer;
  writePPMP3_p: Colour;
  writePPMP3_out: string;
  writePPMP3_numsize: integer;
  writePPMP3_line: string;
begin
  writePPMP3_maxv := 0;
  y := 0;
  while y < b.height do begin
  x := 0;
  while x < b.width do begin
  writePPMP3_p := b.pixels[y][x];
  if writePPMP3_p.R > writePPMP3_maxv then begin
  writePPMP3_maxv := writePPMP3_p.R;
end;
  if writePPMP3_p.G > writePPMP3_maxv then begin
  writePPMP3_maxv := writePPMP3_p.G;
end;
  if writePPMP3_p.B > writePPMP3_maxv then begin
  writePPMP3_maxv := writePPMP3_p.B;
end;
  x := x + 1;
end;
  y := y + 1;
end;
  writePPMP3_out := ((((('P3' + #10 + '# generated from Bitmap.writeppmp3' + #10 + '' + IntToStr(b.width)) + ' ') + IntToStr(b.height)) + '' + #10 + '') + IntToStr(writePPMP3_maxv)) + '' + #10 + '';
  writePPMP3_numsize := Length(IntToStr(writePPMP3_maxv));
  y := b.height - 1;
  while y >= 0 do begin
  writePPMP3_line := '';
  x := 0;
  while x < b.width do begin
  writePPMP3_p := b.pixels[y][x];
  writePPMP3_line := (((((writePPMP3_line + '   ') + pad(writePPMP3_p.R, writePPMP3_numsize)) + ' ') + pad(writePPMP3_p.G, writePPMP3_numsize)) + ' ') + pad(writePPMP3_p.B, writePPMP3_numsize);
  x := x + 1;
end;
  writePPMP3_out := writePPMP3_out + writePPMP3_line;
  if y > 0 then begin
  writePPMP3_out := writePPMP3_out + '' + #10 + '';
end else begin
  writePPMP3_out := writePPMP3_out + '' + #10 + '';
end;
  y := y - 1;
end;
  exit(writePPMP3_out);
end;
procedure main();
var
  main_black: Colour;
  main_white: Colour;
  main_bm: Bitmap;
  main_ppm: string;
begin
  main_black := makeColour(0, 0, 0);
  main_white := makeColour(255, 255, 255);
  main_bm := newBitmap(4, 4, main_black);
  fillRect(main_bm, 1, 0, 1, 2, main_white);
  setPixel(main_bm, 3, 3, makeColour(127, 0, 63));
  main_ppm := writePPMP3(main_bm);
  writeln(main_ppm);
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
