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
  sample: array of IntArray;
  img: IntArrayArray;
  value: integer;
  level: integer;
function clamp(value: integer): integer; forward;
function change_brightness(img: IntArrayArray; level: integer): IntArrayArray; forward;
function clamp(value: integer): integer;
begin
  if value < 0 then begin
  exit(0);
end;
  if value > 255 then begin
  exit(255);
end;
  exit(value);
end;
function change_brightness(img: IntArrayArray; level: integer): IntArrayArray;
var
  change_brightness_result_: array of IntArray;
  change_brightness_i: integer;
  change_brightness_row_res: array of integer;
  change_brightness_j: integer;
begin
  if (level < -255) or (level > 255) then begin
  panic('level must be between -255 and 255');
end;
  change_brightness_result_ := [];
  change_brightness_i := 0;
  while change_brightness_i < Length(img) do begin
  change_brightness_row_res := [];
  change_brightness_j := 0;
  while change_brightness_j < Length(img[change_brightness_i]) do begin
  change_brightness_row_res := concat(change_brightness_row_res, IntArray([clamp(img[change_brightness_i][change_brightness_j] + level)]));
  change_brightness_j := change_brightness_j + 1;
end;
  change_brightness_result_ := concat(change_brightness_result_, [change_brightness_row_res]);
  change_brightness_i := change_brightness_i + 1;
end;
  exit(change_brightness_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  sample := [[100, 150], [200, 250]];
  show_list_list(change_brightness(sample, 30));
  show_list_list(change_brightness(sample, -60));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
