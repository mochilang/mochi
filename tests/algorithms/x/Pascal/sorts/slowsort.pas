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
  seq1: array of integer;
  seq2: array of integer;
  seq3: array of integer;
  seq4: array of integer;
  seq5: array of integer;
  seq6: array of integer;
  seq7: array of integer;
  seq8: array of integer;
  j: integer;
  end_index: integer;
  seq: IntArray;
  i: integer;
  start: integer;
procedure swap(seq: IntArray; i: integer; j: integer); forward;
procedure slowsort_recursive(seq: IntArray; start: integer; end_index: integer); forward;
function slow_sort(seq: IntArray): IntArray; forward;
procedure swap(seq: IntArray; i: integer; j: integer);
var
  swap_temp: integer;
begin
  swap_temp := seq[i];
  seq[i] := seq[j];
  seq[j] := swap_temp;
end;
procedure slowsort_recursive(seq: IntArray; start: integer; end_index: integer);
var
  slowsort_recursive_mid: integer;
begin
  if start >= end_index then begin
  exit();
end;
  slowsort_recursive_mid := (start + end_index) div 2;
  slowsort_recursive(seq, start, slowsort_recursive_mid);
  slowsort_recursive(seq, slowsort_recursive_mid + 1, end_index);
  if seq[end_index] < seq[slowsort_recursive_mid] then begin
  swap(seq, end_index, slowsort_recursive_mid);
end;
  slowsort_recursive(seq, start, end_index - 1);
end;
function slow_sort(seq: IntArray): IntArray;
begin
  if Length(seq) > 0 then begin
  slowsort_recursive(seq, 0, Length(seq) - 1);
end;
  exit(seq);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seq1 := [1, 6, 2, 5, 3, 4, 4, 5];
  writeln(list_int_to_str(slow_sort(seq1)));
  seq2 := [];
  writeln(list_int_to_str(slow_sort(seq2)));
  seq3 := [2];
  writeln(list_int_to_str(slow_sort(seq3)));
  seq4 := [1, 2, 3, 4];
  writeln(list_int_to_str(slow_sort(seq4)));
  seq5 := [4, 3, 2, 1];
  writeln(list_int_to_str(slow_sort(seq5)));
  seq6 := [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
  slowsort_recursive(seq6, 2, 7);
  writeln(list_int_to_str(seq6));
  seq7 := [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
  slowsort_recursive(seq7, 0, 4);
  writeln(list_int_to_str(seq7));
  seq8 := [9, 8, 7, 6, 5, 4, 3, 2, 1, 0];
  slowsort_recursive(seq8, 5, Length(seq8) - 1);
  writeln(list_int_to_str(seq8));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
