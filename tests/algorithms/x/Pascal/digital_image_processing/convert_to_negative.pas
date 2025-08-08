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
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  img: IntArrayArrayArray;
function convert_to_negative(img: IntArrayArrayArray): IntArrayArrayArray; forward;
procedure main(); forward;
function convert_to_negative(img: IntArrayArrayArray): IntArrayArrayArray;
var
  convert_to_negative_result_: array of IntArrayArray;
  convert_to_negative_i: integer;
  convert_to_negative_row: array of IntArray;
  convert_to_negative_j: integer;
  convert_to_negative_pixel: array of integer;
  convert_to_negative_r: integer;
  convert_to_negative_g: integer;
  convert_to_negative_b: integer;
begin
  convert_to_negative_result_ := [];
  convert_to_negative_i := 0;
  while convert_to_negative_i < Length(img) do begin
  convert_to_negative_row := [];
  convert_to_negative_j := 0;
  while convert_to_negative_j < Length(img[convert_to_negative_i]) do begin
  convert_to_negative_pixel := img[convert_to_negative_i][convert_to_negative_j];
  convert_to_negative_r := 255 - convert_to_negative_pixel[0];
  convert_to_negative_g := 255 - convert_to_negative_pixel[1];
  convert_to_negative_b := 255 - convert_to_negative_pixel[2];
  convert_to_negative_row := concat(convert_to_negative_row, [[convert_to_negative_r, convert_to_negative_g, convert_to_negative_b]]);
  convert_to_negative_j := convert_to_negative_j + 1;
end;
  convert_to_negative_result_ := concat(convert_to_negative_result_, [convert_to_negative_row]);
  convert_to_negative_i := convert_to_negative_i + 1;
end;
  exit(convert_to_negative_result_);
end;
procedure main();
var
  main_image: array of array of array of integer;
  main_neg: IntArrayArrayArray;
begin
  main_image := [[[10, 20, 30], [0, 0, 0]], [[255, 255, 255], [100, 150, 200]]];
  main_neg := convert_to_negative(main_image);
  show_list_list(main_neg);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
