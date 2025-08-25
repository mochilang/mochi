{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
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
function bubble_sort(bubble_sort_nums: IntArray): IntArray; forward;
function three_sum(three_sum_nums: IntArray): IntArrayArray; forward;
function bubble_sort(bubble_sort_nums: IntArray): IntArray;
var
  bubble_sort_arr: array of int64;
  bubble_sort_n: integer;
  bubble_sort_i: int64;
  bubble_sort_j: int64;
  bubble_sort_temp: int64;
begin
  bubble_sort_arr := bubble_sort_nums;
  bubble_sort_n := Length(bubble_sort_arr);
  bubble_sort_i := 0;
  while bubble_sort_i < bubble_sort_n do begin
  bubble_sort_j := 0;
  while bubble_sort_j < (bubble_sort_n - 1) do begin
  if bubble_sort_arr[bubble_sort_j] > bubble_sort_arr[bubble_sort_j + 1] then begin
  bubble_sort_temp := bubble_sort_arr[bubble_sort_j];
  bubble_sort_arr[bubble_sort_j] := bubble_sort_arr[bubble_sort_j + 1];
  bubble_sort_arr[bubble_sort_j + 1] := bubble_sort_temp;
end;
  bubble_sort_j := bubble_sort_j + 1;
end;
  bubble_sort_i := bubble_sort_i + 1;
end;
  exit(bubble_sort_arr);
end;
function three_sum(three_sum_nums: IntArray): IntArrayArray;
var
  three_sum_sorted: IntArray;
  three_sum_res: array of IntArray;
  three_sum_n: integer;
  three_sum_i: int64;
  three_sum_low: int64;
  three_sum_high: integer;
  three_sum_c: int64;
  three_sum_s: int64;
  three_sum_triple: array of int64;
begin
  three_sum_sorted := bubble_sort(three_sum_nums);
  three_sum_res := [];
  three_sum_n := Length(three_sum_sorted);
  three_sum_i := 0;
  while three_sum_i < (three_sum_n - 2) do begin
  if (three_sum_i = 0) or (three_sum_sorted[three_sum_i] <> three_sum_sorted[three_sum_i - 1]) then begin
  three_sum_low := three_sum_i + 1;
  three_sum_high := three_sum_n - 1;
  three_sum_c := 0 - three_sum_sorted[three_sum_i];
  while three_sum_low < three_sum_high do begin
  three_sum_s := three_sum_sorted[three_sum_low] + three_sum_sorted[three_sum_high];
  if three_sum_s = three_sum_c then begin
  three_sum_triple := [three_sum_sorted[three_sum_i], three_sum_sorted[three_sum_low], three_sum_sorted[three_sum_high]];
  three_sum_res := concat(three_sum_res, [three_sum_triple]);
  while (three_sum_low < three_sum_high) and (three_sum_sorted[three_sum_low] = three_sum_sorted[three_sum_low + 1]) do begin
  three_sum_low := three_sum_low + 1;
end;
  while (three_sum_low < three_sum_high) and (three_sum_sorted[three_sum_high] = three_sum_sorted[three_sum_high - 1]) do begin
  three_sum_high := three_sum_high - 1;
end;
  three_sum_low := three_sum_low + 1;
  three_sum_high := three_sum_high - 1;
end else begin
  if three_sum_s < three_sum_c then begin
  three_sum_low := three_sum_low + 1;
end else begin
  three_sum_high := three_sum_high - 1;
end;
end;
end;
end;
  three_sum_i := three_sum_i + 1;
end;
  exit(three_sum_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(three_sum([-1, 0, 1, 2, -1, -4])));
  writeln(list_list_int_to_str(three_sum([1, 2, 3, 4])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
