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
  a: IntArray;
  gray_img: IntArrayArray;
  mask: integer;
function insertion_sort(a: IntArray): IntArray; forward;
function median_filter(gray_img: IntArrayArray; mask: integer): IntArrayArray; forward;
procedure main(); forward;
function insertion_sort(a: IntArray): IntArray;
var
  insertion_sort_i: integer;
  insertion_sort_key: integer;
  insertion_sort_j: integer;
begin
  insertion_sort_i := 1;
  while insertion_sort_i < Length(a) do begin
  insertion_sort_key := a[insertion_sort_i];
  insertion_sort_j := insertion_sort_i - 1;
  while (insertion_sort_j >= 0) and (a[insertion_sort_j] > insertion_sort_key) do begin
  a[insertion_sort_j + 1] := a[insertion_sort_j];
  insertion_sort_j := insertion_sort_j - 1;
end;
  a[insertion_sort_j + 1] := insertion_sort_key;
  insertion_sort_i := insertion_sort_i + 1;
end;
  exit(a);
end;
function median_filter(gray_img: IntArrayArray; mask: integer): IntArrayArray;
var
  median_filter_rows: integer;
  median_filter_cols: integer;
  median_filter_bd: integer;
  median_filter_result_: array of IntArray;
  median_filter_i: integer;
  median_filter_row: array of integer;
  median_filter_j: integer;
  median_filter_kernel: array of integer;
  median_filter_x: integer;
  median_filter_y: integer;
  median_filter_idx: integer;
begin
  median_filter_rows := Length(gray_img);
  median_filter_cols := Length(gray_img[0]);
  median_filter_bd := mask div 2;
  median_filter_result_ := [];
  median_filter_i := 0;
  while median_filter_i < median_filter_rows do begin
  median_filter_row := [];
  median_filter_j := 0;
  while median_filter_j < median_filter_cols do begin
  median_filter_row := concat(median_filter_row, IntArray([0]));
  median_filter_j := median_filter_j + 1;
end;
  median_filter_result_ := concat(median_filter_result_, [median_filter_row]);
  median_filter_i := median_filter_i + 1;
end;
  median_filter_i := median_filter_bd;
  while median_filter_i < (median_filter_rows - median_filter_bd) do begin
  median_filter_j := median_filter_bd;
  while median_filter_j < (median_filter_cols - median_filter_bd) do begin
  median_filter_kernel := [];
  median_filter_x := median_filter_i - median_filter_bd;
  while median_filter_x <= (median_filter_i + median_filter_bd) do begin
  median_filter_y := median_filter_j - median_filter_bd;
  while median_filter_y <= (median_filter_j + median_filter_bd) do begin
  median_filter_kernel := concat(median_filter_kernel, IntArray([gray_img[median_filter_x][median_filter_y]]));
  median_filter_y := median_filter_y + 1;
end;
  median_filter_x := median_filter_x + 1;
end;
  median_filter_kernel := insertion_sort(median_filter_kernel);
  median_filter_idx := (mask * mask) div 2;
  median_filter_result_[median_filter_i][median_filter_j] := median_filter_kernel[median_filter_idx];
  median_filter_j := median_filter_j + 1;
end;
  median_filter_i := median_filter_i + 1;
end;
  exit(median_filter_result_);
end;
procedure main();
var
  main_img: array of IntArray;
  main_filtered: IntArrayArray;
begin
  main_img := [[10, 10, 10, 10, 10], [10, 255, 10, 255, 10], [10, 10, 10, 10, 10], [10, 255, 10, 255, 10], [10, 10, 10, 10, 10]];
  main_filtered := median_filter(main_img, 3);
  show_list_list(main_filtered);
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
