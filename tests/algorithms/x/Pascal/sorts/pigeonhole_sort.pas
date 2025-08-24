{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function min(xs: array of int64): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin min := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] < m then m := xs[i];
  min := m;
end;
function max(xs: array of int64): integer;
var i, m: integer;
begin
  if Length(xs) = 0 then begin max := 0; exit; end;
  m := xs[0];
  for i := 1 to High(xs) do if xs[i] > m then m := xs[i];
  max := m;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  example: array of int64;
  result_: IntArray;
  output: string;
  j: int64;
function pigeonhole_sort(pigeonhole_sort_arr: IntArray): IntArray; forward;
function pigeonhole_sort(pigeonhole_sort_arr: IntArray): IntArray;
var
  pigeonhole_sort_min_val: integer;
  pigeonhole_sort_max_val: integer;
  pigeonhole_sort_size: int64;
  pigeonhole_sort_holes: array of int64;
  pigeonhole_sort_i: int64;
  pigeonhole_sort_x: int64;
  pigeonhole_sort_index: int64;
  pigeonhole_sort_sorted_index: int64;
  pigeonhole_sort_count: int64;
begin
  if Length(pigeonhole_sort_arr) = 0 then begin
  exit(pigeonhole_sort_arr);
end;
  pigeonhole_sort_min_val := Trunc(min(pigeonhole_sort_arr));
  pigeonhole_sort_max_val := Trunc(max(pigeonhole_sort_arr));
  pigeonhole_sort_size := (pigeonhole_sort_max_val - pigeonhole_sort_min_val) + 1;
  pigeonhole_sort_holes := [];
  pigeonhole_sort_i := 0;
  while pigeonhole_sort_i < pigeonhole_sort_size do begin
  pigeonhole_sort_holes := concat(pigeonhole_sort_holes, IntArray([0]));
  pigeonhole_sort_i := pigeonhole_sort_i + 1;
end;
  pigeonhole_sort_i := 0;
  while pigeonhole_sort_i < Length(pigeonhole_sort_arr) do begin
  pigeonhole_sort_x := pigeonhole_sort_arr[pigeonhole_sort_i];
  pigeonhole_sort_index := pigeonhole_sort_x - pigeonhole_sort_min_val;
  pigeonhole_sort_holes[pigeonhole_sort_index] := pigeonhole_sort_holes[pigeonhole_sort_index] + 1;
  pigeonhole_sort_i := pigeonhole_sort_i + 1;
end;
  pigeonhole_sort_sorted_index := 0;
  pigeonhole_sort_count := 0;
  while pigeonhole_sort_count < pigeonhole_sort_size do begin
  while pigeonhole_sort_holes[pigeonhole_sort_count] > 0 do begin
  pigeonhole_sort_arr[pigeonhole_sort_sorted_index] := pigeonhole_sort_count + pigeonhole_sort_min_val;
  pigeonhole_sort_holes[pigeonhole_sort_count] := pigeonhole_sort_holes[pigeonhole_sort_count] - 1;
  pigeonhole_sort_sorted_index := pigeonhole_sort_sorted_index + 1;
end;
  pigeonhole_sort_count := pigeonhole_sort_count + 1;
end;
  exit(pigeonhole_sort_arr);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example := [8, 3, 2, 7, 4, 6, 8];
  result_ := pigeonhole_sort(example);
  output := 'Sorted order is:';
  j := 0;
  while j < Length(result_) do begin
  output := (output + ' ') + IntToStr(result_[j]);
  j := j + 1;
end;
  writeln(output);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
