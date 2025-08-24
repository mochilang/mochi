{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function swap(swap_xs: RealArray; swap_i: int64; swap_j: int64): RealArray; forward;
function wiggle_sort(wiggle_sort_nums: RealArray): RealArray; forward;
function swap(swap_xs: RealArray; swap_i: int64; swap_j: int64): RealArray;
var
  swap_res: array of real;
  swap_k: int64;
begin
  swap_res := [];
  swap_k := 0;
  while swap_k < Length(swap_xs) do begin
  if swap_k = swap_i then begin
  swap_res := concat(swap_res, [swap_xs[swap_j]]);
end else begin
  if swap_k = swap_j then begin
  swap_res := concat(swap_res, [swap_xs[swap_i]]);
end else begin
  swap_res := concat(swap_res, [swap_xs[swap_k]]);
end;
end;
  swap_k := swap_k + 1;
end;
  exit(swap_res);
end;
function wiggle_sort(wiggle_sort_nums: RealArray): RealArray;
var
  wiggle_sort_i: int64;
  wiggle_sort_res: array of real;
  wiggle_sort_j: integer;
  wiggle_sort_prev: real;
  wiggle_sort_curr: real;
begin
  wiggle_sort_i := 0;
  wiggle_sort_res := wiggle_sort_nums;
  while wiggle_sort_i < Length(wiggle_sort_res) do begin
  if wiggle_sort_i = 0 then begin
  wiggle_sort_j := Length(wiggle_sort_res) - 1;
end else begin
  wiggle_sort_j := wiggle_sort_i - 1;
end;
  wiggle_sort_prev := wiggle_sort_res[wiggle_sort_j];
  wiggle_sort_curr := wiggle_sort_res[wiggle_sort_i];
  if ((wiggle_sort_i mod 2) = 1) = (wiggle_sort_prev > wiggle_sort_curr) then begin
  wiggle_sort_res := swap(wiggle_sort_res, wiggle_sort_j, wiggle_sort_i);
end;
  wiggle_sort_i := wiggle_sort_i + 1;
end;
  exit(wiggle_sort_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_real_to_str(wiggle_sort([3, 5, 2, 1, 6, 4])));
  writeln(list_real_to_str(wiggle_sort([0, 5, 3, 2, 2])));
  writeln(list_real_to_str(wiggle_sort(RealArray([]))));
  writeln(list_real_to_str(wiggle_sort([-2, -5, -45])));
  writeln(list_real_to_str(wiggle_sort([-2.1, -5.68, -45.11])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
