{$mode objfpc}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function sel(list: RealArray; k: integer): real; forward;
function median(a: RealArray): real; forward;
function sel(list: RealArray; k: integer): real;
var
  sel_i: integer;
  sel_minIndex: integer;
  sel_j: integer;
  sel_tmp: real;
begin
  sel_i := 0;
  while sel_i <= k do begin
  sel_minIndex := sel_i;
  sel_j := sel_i + 1;
  while sel_j < Length(list) do begin
  if list[sel_j] < list[sel_minIndex] then begin
  sel_minIndex := sel_j;
end;
  sel_j := sel_j + 1;
end;
  sel_tmp := list[sel_i];
  list[sel_i] := list[sel_minIndex];
  list[sel_minIndex] := sel_tmp;
  sel_i := sel_i + 1;
end;
  exit(list[k]);
end;
function median(a: RealArray): real;
var
  median_arr: array of real;
  median_half: integer;
  median_med: real;
begin
  median_arr := a;
  median_half := Trunc(Length(median_arr) div 2);
  median_med := sel(median_arr, median_half);
  if (Length(median_arr) mod 2) = 0 then begin
  exit((median_med + median_arr[median_half - 1]) / 2);
end;
  exit(median_med);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(median([3, 1, 4, 1])));
  writeln(FloatToStr(median([3, 1, 4, 1, 5])));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
