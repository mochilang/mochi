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
  w: integer;
  c: integer;
  dst_w: integer;
  h: integer;
  img: IntArrayArrayArray;
  dst_h: integer;
function zeros3d(h: integer; w: integer; c: integer): IntArrayArrayArray; forward;
function resize_nn(img: IntArrayArrayArray; dst_w: integer; dst_h: integer): IntArrayArrayArray; forward;
procedure main(); forward;
function zeros3d(h: integer; w: integer; c: integer): IntArrayArrayArray;
var
  zeros3d_arr: array of IntArrayArray;
  zeros3d_y: integer;
  zeros3d_row: array of IntArray;
  zeros3d_x: integer;
  zeros3d_pixel: array of integer;
  zeros3d_k: integer;
begin
  zeros3d_arr := [];
  zeros3d_y := 0;
  while zeros3d_y < h do begin
  zeros3d_row := [];
  zeros3d_x := 0;
  while zeros3d_x < w do begin
  zeros3d_pixel := [];
  zeros3d_k := 0;
  while zeros3d_k < c do begin
  zeros3d_pixel := concat(zeros3d_pixel, IntArray([0]));
  zeros3d_k := zeros3d_k + 1;
end;
  zeros3d_row := concat(zeros3d_row, [zeros3d_pixel]);
  zeros3d_x := zeros3d_x + 1;
end;
  zeros3d_arr := concat(zeros3d_arr, [zeros3d_row]);
  zeros3d_y := zeros3d_y + 1;
end;
  exit(zeros3d_arr);
end;
function resize_nn(img: IntArrayArrayArray; dst_w: integer; dst_h: integer): IntArrayArrayArray;
var
  resize_nn_src_h: integer;
  resize_nn_src_w: integer;
  resize_nn_channels: integer;
  resize_nn_ratio_x: real;
  resize_nn_ratio_y: real;
  resize_nn_out: IntArrayArrayArray;
  resize_nn_i: integer;
  resize_nn_j: integer;
  resize_nn_src_x: integer;
  resize_nn_src_y: integer;
begin
  resize_nn_src_h := Length(img);
  resize_nn_src_w := Length(img[0]);
  resize_nn_channels := Length(img[0][0]);
  resize_nn_ratio_x := Double(resize_nn_src_w) / Double(dst_w);
  resize_nn_ratio_y := Double(resize_nn_src_h) / Double(dst_h);
  resize_nn_out := zeros3d(dst_h, dst_w, resize_nn_channels);
  resize_nn_i := 0;
  while resize_nn_i < dst_h do begin
  resize_nn_j := 0;
  while resize_nn_j < dst_w do begin
  resize_nn_src_x := Trunc(resize_nn_ratio_x * Double(resize_nn_j));
  resize_nn_src_y := Trunc(resize_nn_ratio_y * Double(resize_nn_i));
  resize_nn_out[resize_nn_i][resize_nn_j] := img[resize_nn_src_y][resize_nn_src_x];
  resize_nn_j := resize_nn_j + 1;
end;
  resize_nn_i := resize_nn_i + 1;
end;
  exit(resize_nn_out);
end;
procedure main();
var
  main_img: array of IntArrayArray;
  main_resized: IntArrayArrayArray;
begin
  main_img := [[[0, 0, 0], [255, 255, 255]], [[255, 0, 0], [0, 255, 0]]];
  main_resized := resize_nn(main_img, 4, 4);
  show_list_list(main_resized);
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
