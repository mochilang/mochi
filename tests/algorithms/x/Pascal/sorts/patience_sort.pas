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
function bisect_left(bisect_left_stacks: IntArrayArray; bisect_left_value: int64): int64; forward;
function reverse_list(reverse_list_src: IntArray): IntArray; forward;
function patience_sort(patience_sort_collection: IntArray): IntArray; forward;
function bisect_left(bisect_left_stacks: IntArrayArray; bisect_left_value: int64): int64;
var
  bisect_left_low: int64;
  bisect_left_high: integer;
  bisect_left_mid: int64;
  bisect_left_stack: array of int64;
  bisect_left_top_idx: integer;
  bisect_left_top: int64;
begin
  bisect_left_low := 0;
  bisect_left_high := Length(bisect_left_stacks);
  while bisect_left_low < bisect_left_high do begin
  bisect_left_mid := (bisect_left_low + bisect_left_high) div 2;
  bisect_left_stack := bisect_left_stacks[bisect_left_mid];
  bisect_left_top_idx := Length(bisect_left_stack) - 1;
  bisect_left_top := bisect_left_stack[bisect_left_top_idx];
  if bisect_left_top < bisect_left_value then begin
  bisect_left_low := bisect_left_mid + 1;
end else begin
  bisect_left_high := bisect_left_mid;
end;
end;
  exit(bisect_left_low);
end;
function reverse_list(reverse_list_src: IntArray): IntArray;
var
  reverse_list_res: array of int64;
  reverse_list_i: integer;
begin
  reverse_list_res := [];
  reverse_list_i := Length(reverse_list_src) - 1;
  while reverse_list_i >= 0 do begin
  reverse_list_res := concat(reverse_list_res, IntArray([reverse_list_src[reverse_list_i]]));
  reverse_list_i := reverse_list_i - 1;
end;
  exit(reverse_list_res);
end;
function patience_sort(patience_sort_collection: IntArray): IntArray;
var
  patience_sort_stacks: array of IntArray;
  patience_sort_i: int64;
  patience_sort_element: int64;
  patience_sort_idx: int64;
  patience_sort_stack: array of int64;
  patience_sort_new_stack: array of int64;
  patience_sort_indices: array of int64;
  patience_sort_total: int64;
  patience_sort_result_: array of int64;
  patience_sort_count: int64;
  patience_sort_min_val: int64;
  patience_sort_min_stack: int64;
  patience_sort_j: int64;
  patience_sort_val: int64;
begin
  patience_sort_stacks := [];
  patience_sort_i := 0;
  while patience_sort_i < Length(patience_sort_collection) do begin
  patience_sort_element := patience_sort_collection[patience_sort_i];
  patience_sort_idx := bisect_left(patience_sort_stacks, patience_sort_element);
  if patience_sort_idx <> Length(patience_sort_stacks) then begin
  patience_sort_stack := patience_sort_stacks[patience_sort_idx];
  patience_sort_stacks[patience_sort_idx] := concat(patience_sort_stack, IntArray([patience_sort_element]));
end else begin
  patience_sort_new_stack := [patience_sort_element];
  patience_sort_stacks := concat(patience_sort_stacks, [patience_sort_new_stack]);
end;
  patience_sort_i := patience_sort_i + 1;
end;
  patience_sort_i := 0;
  while patience_sort_i < Length(patience_sort_stacks) do begin
  patience_sort_stacks[patience_sort_i] := reverse_list(patience_sort_stacks[patience_sort_i]);
  patience_sort_i := patience_sort_i + 1;
end;
  patience_sort_indices := [];
  patience_sort_i := 0;
  while patience_sort_i < Length(patience_sort_stacks) do begin
  patience_sort_indices := concat(patience_sort_indices, IntArray([0]));
  patience_sort_i := patience_sort_i + 1;
end;
  patience_sort_total := 0;
  patience_sort_i := 0;
  while patience_sort_i < Length(patience_sort_stacks) do begin
  patience_sort_total := patience_sort_total + Length(patience_sort_stacks[patience_sort_i]);
  patience_sort_i := patience_sort_i + 1;
end;
  patience_sort_result_ := [];
  patience_sort_count := 0;
  while patience_sort_count < patience_sort_total do begin
  patience_sort_min_val := 0;
  patience_sort_min_stack := -1;
  patience_sort_j := 0;
  while patience_sort_j < Length(patience_sort_stacks) do begin
  patience_sort_idx := patience_sort_indices[patience_sort_j];
  if patience_sort_idx < Length(patience_sort_stacks[patience_sort_j]) then begin
  patience_sort_val := patience_sort_stacks[patience_sort_j][patience_sort_idx];
  if patience_sort_min_stack < 0 then begin
  patience_sort_min_val := patience_sort_val;
  patience_sort_min_stack := patience_sort_j;
end else begin
  if patience_sort_val < patience_sort_min_val then begin
  patience_sort_min_val := patience_sort_val;
  patience_sort_min_stack := patience_sort_j;
end;
end;
end;
  patience_sort_j := patience_sort_j + 1;
end;
  patience_sort_result_ := concat(patience_sort_result_, IntArray([patience_sort_min_val]));
  patience_sort_indices[patience_sort_min_stack] := patience_sort_indices[patience_sort_min_stack] + 1;
  patience_sort_count := patience_sort_count + 1;
end;
  patience_sort_i := 0;
  while patience_sort_i < Length(patience_sort_result_) do begin
  patience_sort_collection[patience_sort_i] := patience_sort_result_[patience_sort_i];
  patience_sort_i := patience_sort_i + 1;
end;
  exit(patience_sort_collection);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(patience_sort([1, 9, 5, 21, 17, 6])));
  writeln(list_int_to_str(patience_sort([])));
  writeln(list_int_to_str(patience_sort([-3, -17, -48])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
