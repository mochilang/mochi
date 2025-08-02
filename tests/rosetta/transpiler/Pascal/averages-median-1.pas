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
function sortFloat(xs: RealArray): RealArray; forward;
function median(a: RealArray): real; forward;
function sortFloat(xs: RealArray): RealArray;
var
  sortFloat_arr: array of real;
  sortFloat_n: integer;
  sortFloat_i: integer;
  sortFloat_j: integer;
  sortFloat_tmp: real;
begin
  sortFloat_arr := xs;
  sortFloat_n := Length(sortFloat_arr);
  sortFloat_i := 0;
  while sortFloat_i < sortFloat_n do begin
  sortFloat_j := 0;
  while sortFloat_j < (sortFloat_n - 1) do begin
  if sortFloat_arr[sortFloat_j] > sortFloat_arr[sortFloat_j + 1] then begin
  sortFloat_tmp := sortFloat_arr[sortFloat_j];
  sortFloat_arr[sortFloat_j] := sortFloat_arr[sortFloat_j + 1];
  sortFloat_arr[sortFloat_j + 1] := sortFloat_tmp;
end;
  sortFloat_j := sortFloat_j + 1;
end;
  sortFloat_i := sortFloat_i + 1;
end;
  exit(sortFloat_arr);
end;
function median(a: RealArray): real;
var
  median_arr: RealArray;
  median_half: integer;
  median_m: real;
begin
  median_arr := sortFloat(a);
  median_half := Trunc(Length(median_arr) div 2);
  median_m := median_arr[median_half];
  if (Length(median_arr) mod 2) = 0 then begin
  median_m := (median_m + median_arr[median_half - 1]) / 2;
end;
  exit(median_m);
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
