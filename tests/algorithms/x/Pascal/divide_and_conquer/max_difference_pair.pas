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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  start: integer;
  end_: integer;
  a: IntArray;
function min_slice(a: IntArray; start: integer; end_: integer): integer; forward;
function max_slice(a: IntArray; start: integer; end_: integer): integer; forward;
function max_diff_range(a: IntArray; start: integer; end_: integer): IntArray; forward;
function max_difference(a: IntArray): IntArray; forward;
procedure main(); forward;
function min_slice(a: IntArray; start: integer; end_: integer): integer;
var
  min_slice_m: integer;
  min_slice_i: integer;
begin
  min_slice_m := a[start];
  min_slice_i := start + 1;
  while min_slice_i < end_ do begin
  if a[min_slice_i] < min_slice_m then begin
  min_slice_m := a[min_slice_i];
end;
  min_slice_i := min_slice_i + 1;
end;
  exit(min_slice_m);
end;
function max_slice(a: IntArray; start: integer; end_: integer): integer;
var
  max_slice_m: integer;
  max_slice_i: integer;
begin
  max_slice_m := a[start];
  max_slice_i := start + 1;
  while max_slice_i < end_ do begin
  if a[max_slice_i] > max_slice_m then begin
  max_slice_m := a[max_slice_i];
end;
  max_slice_i := max_slice_i + 1;
end;
  exit(max_slice_m);
end;
function max_diff_range(a: IntArray; start: integer; end_: integer): IntArray;
var
  max_diff_range_v: integer;
  max_diff_range_mid: integer;
  max_diff_range_left: array of integer;
  max_diff_range_right: array of integer;
  max_diff_range_small1: integer;
  max_diff_range_big1: integer;
  max_diff_range_small2: integer;
  max_diff_range_big2: integer;
  max_diff_range_min_left: integer;
  max_diff_range_max_right: integer;
  max_diff_range_cross_diff: integer;
  max_diff_range_left_diff: integer;
  max_diff_range_right_diff: integer;
begin
  if (end_ - start) = 1 then begin
  max_diff_range_v := a[start];
  exit([max_diff_range_v, max_diff_range_v]);
end;
  max_diff_range_mid := (start + end_) div 2;
  max_diff_range_left := max_diff_range(a, start, max_diff_range_mid);
  max_diff_range_right := max_diff_range(a, max_diff_range_mid, end_);
  max_diff_range_small1 := max_diff_range_left[0];
  max_diff_range_big1 := max_diff_range_left[1];
  max_diff_range_small2 := max_diff_range_right[0];
  max_diff_range_big2 := max_diff_range_right[1];
  max_diff_range_min_left := min_slice(a, start, max_diff_range_mid);
  max_diff_range_max_right := max_slice(a, max_diff_range_mid, end_);
  max_diff_range_cross_diff := max_diff_range_max_right - max_diff_range_min_left;
  max_diff_range_left_diff := max_diff_range_big1 - max_diff_range_small1;
  max_diff_range_right_diff := max_diff_range_big2 - max_diff_range_small2;
  if (max_diff_range_right_diff > max_diff_range_cross_diff) and (max_diff_range_right_diff > max_diff_range_left_diff) then begin
  exit([max_diff_range_small2, max_diff_range_big2]);
end else begin
  if max_diff_range_left_diff > max_diff_range_cross_diff then begin
  exit([max_diff_range_small1, max_diff_range_big1]);
end else begin
  exit([max_diff_range_min_left, max_diff_range_max_right]);
end;
end;
end;
function max_difference(a: IntArray): IntArray;
begin
  exit(max_diff_range(a, 0, Length(a)));
end;
procedure main();
var
  main_result_: IntArray;
begin
  main_result_ := max_difference([5, 11, 2, 1, 7, 9, 0, 7]);
  writeln(list_int_to_str(main_result_));
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
