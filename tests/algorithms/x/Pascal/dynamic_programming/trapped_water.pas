{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type IntArray = array of int64;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function make_list(make_list_len: int64; make_list_value: int64): IntArray; forward;
function trapped_rainwater(trapped_rainwater_heights: IntArray): int64; forward;
function make_list(make_list_len: int64; make_list_value: int64): IntArray;
var
  make_list_arr: array of int64;
  make_list_i: int64;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < make_list_len do begin
  make_list_arr := concat(make_list_arr, IntArray([make_list_value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function trapped_rainwater(trapped_rainwater_heights: IntArray): int64;
var
  trapped_rainwater_i: int64;
  trapped_rainwater_length_: int64;
  trapped_rainwater_left_max: IntArray;
  trapped_rainwater_right_max: IntArray;
  trapped_rainwater_last: int64;
  trapped_rainwater_total: int64;
  trapped_rainwater_left: int64;
  trapped_rainwater_right: int64;
  trapped_rainwater_smaller: int64;
begin
  if Length(trapped_rainwater_heights) = 0 then begin
  exit(0);
end;
  trapped_rainwater_i := 0;
  while trapped_rainwater_i < Length(trapped_rainwater_heights) do begin
  if trapped_rainwater_heights[trapped_rainwater_i] < 0 then begin
  panic('No height can be negative');
end;
  trapped_rainwater_i := trapped_rainwater_i + 1;
end;
  trapped_rainwater_length_ := Length(trapped_rainwater_heights);
  trapped_rainwater_left_max := make_list(trapped_rainwater_length_, 0);
  trapped_rainwater_left_max[0] := trapped_rainwater_heights[0];
  trapped_rainwater_i := 1;
  while trapped_rainwater_i < trapped_rainwater_length_ do begin
  if trapped_rainwater_heights[trapped_rainwater_i] > trapped_rainwater_left_max[trapped_rainwater_i - 1] then begin
  trapped_rainwater_left_max[trapped_rainwater_i] := trapped_rainwater_heights[trapped_rainwater_i];
end else begin
  trapped_rainwater_left_max[trapped_rainwater_i] := trapped_rainwater_left_max[trapped_rainwater_i - 1];
end;
  trapped_rainwater_i := trapped_rainwater_i + 1;
end;
  trapped_rainwater_right_max := make_list(trapped_rainwater_length_, 0);
  trapped_rainwater_last := trapped_rainwater_length_ - 1;
  trapped_rainwater_right_max[trapped_rainwater_last] := trapped_rainwater_heights[trapped_rainwater_last];
  trapped_rainwater_i := trapped_rainwater_last - 1;
  while trapped_rainwater_i >= 0 do begin
  if trapped_rainwater_heights[trapped_rainwater_i] > trapped_rainwater_right_max[trapped_rainwater_i + 1] then begin
  trapped_rainwater_right_max[trapped_rainwater_i] := trapped_rainwater_heights[trapped_rainwater_i];
end else begin
  trapped_rainwater_right_max[trapped_rainwater_i] := trapped_rainwater_right_max[trapped_rainwater_i + 1];
end;
  trapped_rainwater_i := trapped_rainwater_i - 1;
end;
  trapped_rainwater_total := 0;
  trapped_rainwater_i := 0;
  while trapped_rainwater_i < trapped_rainwater_length_ do begin
  trapped_rainwater_left := trapped_rainwater_left_max[trapped_rainwater_i];
  trapped_rainwater_right := trapped_rainwater_right_max[trapped_rainwater_i];
  if trapped_rainwater_left < trapped_rainwater_right then begin
  trapped_rainwater_smaller := trapped_rainwater_left;
end else begin
  trapped_rainwater_smaller := trapped_rainwater_right;
end;
  trapped_rainwater_total := trapped_rainwater_total + (trapped_rainwater_smaller - trapped_rainwater_heights[trapped_rainwater_i]);
  trapped_rainwater_i := trapped_rainwater_i + 1;
end;
  exit(trapped_rainwater_total);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(trapped_rainwater([0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1])));
  writeln(IntToStr(trapped_rainwater([7, 1, 5, 3, 6, 4])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
