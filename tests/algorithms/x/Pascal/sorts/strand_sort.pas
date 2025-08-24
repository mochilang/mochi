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
function merge(merge_xs: IntArray; merge_ys: IntArray; merge_reverse: boolean): IntArray; forward;
function strand_sort_rec(strand_sort_rec_arr: IntArray; strand_sort_rec_reverse: boolean; strand_sort_rec_solution: IntArray): IntArray; forward;
function strand_sort(strand_sort_arr: IntArray; strand_sort_reverse: boolean): IntArray; forward;
function merge(merge_xs: IntArray; merge_ys: IntArray; merge_reverse: boolean): IntArray;
var
  merge_result_: array of int64;
  merge_i: int64;
  merge_j: int64;
begin
  merge_result_ := [];
  merge_i := 0;
  merge_j := 0;
  while (merge_i < Length(merge_xs)) and (merge_j < Length(merge_ys)) do begin
  if merge_reverse then begin
  if merge_xs[merge_i] > merge_ys[merge_j] then begin
  merge_result_ := concat(merge_result_, IntArray([merge_xs[merge_i]]));
  merge_i := merge_i + 1;
end else begin
  merge_result_ := concat(merge_result_, IntArray([merge_ys[merge_j]]));
  merge_j := merge_j + 1;
end;
end else begin
  if merge_xs[merge_i] < merge_ys[merge_j] then begin
  merge_result_ := concat(merge_result_, IntArray([merge_xs[merge_i]]));
  merge_i := merge_i + 1;
end else begin
  merge_result_ := concat(merge_result_, IntArray([merge_ys[merge_j]]));
  merge_j := merge_j + 1;
end;
end;
end;
  while merge_i < Length(merge_xs) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_xs[merge_i]]));
  merge_i := merge_i + 1;
end;
  while merge_j < Length(merge_ys) do begin
  merge_result_ := concat(merge_result_, IntArray([merge_ys[merge_j]]));
  merge_j := merge_j + 1;
end;
  exit(merge_result_);
end;
function strand_sort_rec(strand_sort_rec_arr: IntArray; strand_sort_rec_reverse: boolean; strand_sort_rec_solution: IntArray): IntArray;
var
  strand_sort_rec_sublist: array of int64;
  strand_sort_rec_remaining: array of int64;
  strand_sort_rec_last: int64;
  strand_sort_rec_k: int64;
  strand_sort_rec_item: int64;
begin
  if Length(strand_sort_rec_arr) = 0 then begin
  exit(strand_sort_rec_solution);
end;
  strand_sort_rec_sublist := [];
  strand_sort_rec_remaining := [];
  strand_sort_rec_sublist := concat(strand_sort_rec_sublist, IntArray([strand_sort_rec_arr[0]]));
  strand_sort_rec_last := strand_sort_rec_arr[0];
  strand_sort_rec_k := 1;
  while strand_sort_rec_k < Length(strand_sort_rec_arr) do begin
  strand_sort_rec_item := strand_sort_rec_arr[strand_sort_rec_k];
  if strand_sort_rec_reverse then begin
  if strand_sort_rec_item < strand_sort_rec_last then begin
  strand_sort_rec_sublist := concat(strand_sort_rec_sublist, IntArray([strand_sort_rec_item]));
  strand_sort_rec_last := strand_sort_rec_item;
end else begin
  strand_sort_rec_remaining := concat(strand_sort_rec_remaining, IntArray([strand_sort_rec_item]));
end;
end else begin
  if strand_sort_rec_item > strand_sort_rec_last then begin
  strand_sort_rec_sublist := concat(strand_sort_rec_sublist, IntArray([strand_sort_rec_item]));
  strand_sort_rec_last := strand_sort_rec_item;
end else begin
  strand_sort_rec_remaining := concat(strand_sort_rec_remaining, IntArray([strand_sort_rec_item]));
end;
end;
  strand_sort_rec_k := strand_sort_rec_k + 1;
end;
  strand_sort_rec_solution := merge(strand_sort_rec_solution, strand_sort_rec_sublist, strand_sort_rec_reverse);
  exit(strand_sort_rec(strand_sort_rec_remaining, strand_sort_rec_reverse, strand_sort_rec_solution));
end;
function strand_sort(strand_sort_arr: IntArray; strand_sort_reverse: boolean): IntArray;
begin
  exit(strand_sort_rec(strand_sort_arr, strand_sort_reverse, []));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(strand_sort([4, 3, 5, 1, 2], false)));
  writeln(list_int_to_str(strand_sort([4, 3, 5, 1, 2], true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
