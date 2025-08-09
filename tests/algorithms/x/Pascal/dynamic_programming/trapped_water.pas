{$mode objfpc}
program Main;
uses SysUtils, Math;
type IntArray = array of integer;
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
  value: integer;
  len: integer;
  heights: IntArray;
function make_list(len: integer; value: integer): IntArray; forward;
function trapped_rainwater(heights: IntArray): integer; forward;
function make_list(len: integer; value: integer): IntArray;
var
  make_list_arr: array of integer;
  make_list_i: integer;
begin
  make_list_arr := [];
  make_list_i := 0;
  while make_list_i < len do begin
  make_list_arr := concat(make_list_arr, IntArray([value]));
  make_list_i := make_list_i + 1;
end;
  exit(make_list_arr);
end;
function trapped_rainwater(heights: IntArray): integer;
var
  trapped_rainwater_i: integer;
  trapped_rainwater_length_: integer;
  trapped_rainwater_left_max: IntArray;
  trapped_rainwater_right_max: IntArray;
  trapped_rainwater_last: integer;
  trapped_rainwater_total: integer;
  trapped_rainwater_left: integer;
  trapped_rainwater_right: integer;
  trapped_rainwater_smaller: integer;
begin
  if Length(heights) = 0 then begin
  exit(0);
end;
  trapped_rainwater_i := 0;
  while trapped_rainwater_i < Length(heights) do begin
  if heights[trapped_rainwater_i] < 0 then begin
  panic('No height can be negative');
end;
  trapped_rainwater_i := trapped_rainwater_i + 1;
end;
  trapped_rainwater_length_ := Length(heights);
  trapped_rainwater_left_max := make_list(trapped_rainwater_length_, 0);
  trapped_rainwater_left_max[0] := heights[0];
  trapped_rainwater_i := 1;
  while trapped_rainwater_i < trapped_rainwater_length_ do begin
  if heights[trapped_rainwater_i] > trapped_rainwater_left_max[trapped_rainwater_i - 1] then begin
  trapped_rainwater_left_max[trapped_rainwater_i] := heights[trapped_rainwater_i];
end else begin
  trapped_rainwater_left_max[trapped_rainwater_i] := trapped_rainwater_left_max[trapped_rainwater_i - 1];
end;
  trapped_rainwater_i := trapped_rainwater_i + 1;
end;
  trapped_rainwater_right_max := make_list(trapped_rainwater_length_, 0);
  trapped_rainwater_last := trapped_rainwater_length_ - 1;
  trapped_rainwater_right_max[trapped_rainwater_last] := heights[trapped_rainwater_last];
  trapped_rainwater_i := trapped_rainwater_last - 1;
  while trapped_rainwater_i >= 0 do begin
  if heights[trapped_rainwater_i] > trapped_rainwater_right_max[trapped_rainwater_i + 1] then begin
  trapped_rainwater_right_max[trapped_rainwater_i] := heights[trapped_rainwater_i];
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
  trapped_rainwater_total := trapped_rainwater_total + (trapped_rainwater_smaller - heights[trapped_rainwater_i]);
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
end.
