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
function qsel(a: RealArray; k: integer): real; forward;
function median(list: RealArray): real; forward;
function qsel(a: RealArray; k: integer): real;
var
  qsel_arr: array of real;
  qsel_px: integer;
  qsel_pv: real;
  qsel_last: integer;
  qsel_tmp: real;
  qsel_i: integer;
  qsel_v: real;
  qsel_tmp2: real;
begin
  qsel_arr := a;
  while Length(qsel_arr) > 1 do begin
  qsel_px := _now() mod Length(qsel_arr);
  qsel_pv := qsel_arr[qsel_px];
  qsel_last := Length(qsel_arr) - 1;
  qsel_tmp := qsel_arr[qsel_px];
  qsel_arr[qsel_px] := qsel_arr[qsel_last];
  qsel_arr[qsel_last] := qsel_tmp;
  qsel_px := 0;
  qsel_i := 0;
  while qsel_i < qsel_last do begin
  qsel_v := qsel_arr[qsel_i];
  if qsel_v < qsel_pv then begin
  qsel_tmp2 := qsel_arr[qsel_px];
  qsel_arr[qsel_px] := qsel_arr[qsel_i];
  qsel_arr[qsel_i] := qsel_tmp2;
  qsel_px := qsel_px + 1;
end;
  qsel_i := qsel_i + 1;
end;
  if qsel_px = k then begin
  exit(qsel_pv);
end;
  if k < qsel_px then begin
  qsel_arr := copy(qsel_arr, 0, (qsel_px - (0)));
end else begin
  qsel_tmp2 := qsel_arr[qsel_px];
  qsel_arr[qsel_px] := qsel_pv;
  qsel_arr[qsel_last] := qsel_tmp2;
  qsel_arr := copy(qsel_arr, qsel_px + 1, Length(qsel_arr));
  k := k - (qsel_px + 1);
end;
end;
  exit(qsel_arr[0]);
end;
function median(list: RealArray): real;
var
  median_arr: array of real;
  median_half: integer;
  median_med: real;
begin
  median_arr := list;
  median_half := Trunc(Length(median_arr) div 2);
  median_med := qsel(median_arr, median_half);
  if (Length(median_arr) mod 2) = 0 then begin
  exit((median_med + qsel(median_arr, median_half - 1)) / 2);
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
