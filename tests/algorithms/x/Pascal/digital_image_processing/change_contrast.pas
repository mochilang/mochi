{$mode objfpc}
program Main;
uses SysUtils, Math;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  image: array of array of integer;
  contrasted: IntArrayArray;
  level: integer;
  img: IntArrayArray;
function change_contrast(img: IntArrayArray; level: integer): IntArrayArray; forward;
procedure print_image(img: IntArrayArray); forward;
function change_contrast(img: IntArrayArray; level: integer): IntArrayArray;
var
  change_contrast_factor: real;
  change_contrast_result_: array of IntArray;
  change_contrast_i: integer;
  change_contrast_row: array of integer;
  change_contrast_new_row: array of integer;
  change_contrast_j: integer;
  change_contrast_c: integer;
  change_contrast_contrasted: integer;
  change_contrast_clamped: integer;
begin
  change_contrast_factor := (259 * (Double(level) + 255)) / (255 * (259 - Double(level)));
  change_contrast_result_ := [];
  change_contrast_i := 0;
  while change_contrast_i < Length(img) do begin
  change_contrast_row := img[change_contrast_i];
  change_contrast_new_row := [];
  change_contrast_j := 0;
  while change_contrast_j < Length(change_contrast_row) do begin
  change_contrast_c := change_contrast_row[change_contrast_j];
  change_contrast_contrasted := Trunc(128 + (change_contrast_factor * (Double(change_contrast_c) - 128)));
  if change_contrast_contrasted < 0 then begin
  change_contrast_clamped := 0;
end else begin
  if change_contrast_contrasted > 255 then begin
  change_contrast_clamped := 255;
end else begin
  change_contrast_clamped := change_contrast_contrasted;
end;
end;
  change_contrast_new_row := concat(change_contrast_new_row, IntArray([change_contrast_clamped]));
  change_contrast_j := change_contrast_j + 1;
end;
  change_contrast_result_ := concat(change_contrast_result_, [change_contrast_new_row]);
  change_contrast_i := change_contrast_i + 1;
end;
  exit(change_contrast_result_);
end;
procedure print_image(img: IntArrayArray);
var
  print_image_i: integer;
  print_image_row: array of integer;
  print_image_j: integer;
  print_image_line: string;
begin
  print_image_i := 0;
  while print_image_i < Length(img) do begin
  print_image_row := img[print_image_i];
  print_image_j := 0;
  print_image_line := '';
  while print_image_j < Length(print_image_row) do begin
  print_image_line := (print_image_line + IntToStr(print_image_row[print_image_j])) + ' ';
  print_image_j := print_image_j + 1;
end;
  writeln(print_image_line);
  print_image_i := print_image_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  image := [[100, 125, 150], [175, 200, 225], [50, 75, 100]];
  writeln('Original image:');
  print_image(image);
  contrasted := change_contrast(image, 170);
  writeln('After contrast:');
  print_image(contrasted);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
