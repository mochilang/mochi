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
  matrix: IntArrayArray;
  array_: IntArrayArray;
  index: integer;
function iterator_values(matrix: IntArrayArray): IntArray; forward;
function index_2d_array_in_1d(array_: IntArrayArray; index: integer): integer; forward;
function iterator_values(matrix: IntArrayArray): IntArray;
var
  iterator_values_result_: array of integer;
  iterator_values_row: IntArray;
  iterator_values_value: integer;
begin
  iterator_values_result_ := [];
  for iterator_values_row in matrix do begin
  for iterator_values_value in iterator_values_row do begin
  iterator_values_result_ := concat(iterator_values_result_, IntArray([iterator_values_value]));
end;
end;
  exit(iterator_values_result_);
end;
function index_2d_array_in_1d(array_: IntArrayArray; index: integer): integer;
var
  index_2d_array_in_1d_rows: integer;
  index_2d_array_in_1d_cols: integer;
begin
  index_2d_array_in_1d_rows := Length(array_);
  index_2d_array_in_1d_cols := Length(array_[0]);
  if (index_2d_array_in_1d_rows = 0) or (index_2d_array_in_1d_cols = 0) then begin
  panic('no items in array');
end;
  if (index < 0) or (index >= (index_2d_array_in_1d_rows * index_2d_array_in_1d_cols)) then begin
  panic('index out of range');
end;
  exit(array_[Trunc(index div index_2d_array_in_1d_cols)][index mod index_2d_array_in_1d_cols]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(iterator_values([[5], [-523], [-1], [34], [0]])));
  writeln(list_int_to_str(iterator_values([[5, -523, -1], [34, 0]])));
  writeln(IntToStr(index_2d_array_in_1d([[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11]], 5)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
