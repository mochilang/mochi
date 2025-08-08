{$mode objfpc}
program Main;
uses SysUtils;
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
  img: array of array of integer;
  result_: IntArrayArray;
  image: IntArrayArray;
  n: integer;
  value: integer;
function make_list(n: integer; value: integer): IntArray; forward;
function histogram_stretch(image: IntArrayArray): IntArrayArray; forward;
procedure print_image(image: IntArrayArray); forward;
function make_list(n: integer; value: integer): IntArray;
var
  make_list_res: array of integer;
  make_list_i: integer;
begin
  make_list_res := [];
  make_list_i := 0;
  while make_list_i < n do begin
  make_list_res := concat(make_list_res, IntArray([value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_res);
end;
function histogram_stretch(image: IntArrayArray): IntArrayArray;
var
  histogram_stretch_height: integer;
  histogram_stretch_width: integer;
  histogram_stretch_hist: array of integer;
  histogram_stretch_i: integer;
  histogram_stretch_j: integer;
  histogram_stretch_val: integer;
  histogram_stretch_mapping: array of integer;
  histogram_stretch_cumulative: integer;
  histogram_stretch_total: integer;
  histogram_stretch_h: integer;
begin
  histogram_stretch_height := Length(image);
  histogram_stretch_width := Length(image[0]);
  histogram_stretch_hist := make_list(256, 0);
  histogram_stretch_i := 0;
  while histogram_stretch_i < histogram_stretch_height do begin
  histogram_stretch_j := 0;
  while histogram_stretch_j < histogram_stretch_width do begin
  histogram_stretch_val := image[histogram_stretch_i][histogram_stretch_j];
  histogram_stretch_hist[histogram_stretch_val] := histogram_stretch_hist[histogram_stretch_val] + 1;
  histogram_stretch_j := histogram_stretch_j + 1;
end;
  histogram_stretch_i := histogram_stretch_i + 1;
end;
  histogram_stretch_mapping := make_list(256, 0);
  histogram_stretch_cumulative := 0;
  histogram_stretch_total := histogram_stretch_height * histogram_stretch_width;
  histogram_stretch_h := 0;
  while histogram_stretch_h < 256 do begin
  histogram_stretch_cumulative := histogram_stretch_cumulative + histogram_stretch_hist[histogram_stretch_h];
  histogram_stretch_mapping[histogram_stretch_h] := (255 * histogram_stretch_cumulative) div histogram_stretch_total;
  histogram_stretch_h := histogram_stretch_h + 1;
end;
  histogram_stretch_i := 0;
  while histogram_stretch_i < histogram_stretch_height do begin
  histogram_stretch_j := 0;
  while histogram_stretch_j < histogram_stretch_width do begin
  histogram_stretch_val := image[histogram_stretch_i][histogram_stretch_j];
  image[histogram_stretch_i][histogram_stretch_j] := histogram_stretch_mapping[histogram_stretch_val];
  histogram_stretch_j := histogram_stretch_j + 1;
end;
  histogram_stretch_i := histogram_stretch_i + 1;
end;
  exit(image);
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
  img := [[52, 55, 61], [59, 79, 61], [85, 76, 62]];
  result_ := histogram_stretch(img);
  print_image(result_);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
