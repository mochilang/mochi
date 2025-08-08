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
  arr: IntArray;
function copy_list(arr: IntArray): IntArray; forward;
function heaps(arr: IntArray): IntArrayArray; forward;
function copy_list(arr: IntArray): IntArray;
var
  copy_list_result_: array of integer;
  copy_list_i: integer;
begin
  copy_list_result_ := [];
  copy_list_i := 0;
  while copy_list_i < Length(arr) do begin
  copy_list_result_ := concat(copy_list_result_, IntArray([arr[copy_list_i]]));
  copy_list_i := copy_list_i + 1;
end;
  exit(copy_list_result_);
end;
function heaps(arr: IntArray): IntArrayArray;
var
  heaps_single: array of IntArray;
  heaps_n: integer;
  heaps_c: array of integer;
  heaps_i: integer;
  heaps_res: array of IntArray;
  heaps_temp: integer;
begin
  if Length(arr) <= 1 then begin
  heaps_single := [];
  exit(concat(heaps_single, [copy_list(arr)]));
end;
  heaps_n := Length(arr);
  heaps_c := [];
  heaps_i := 0;
  while heaps_i < heaps_n do begin
  heaps_c := concat(heaps_c, IntArray([0]));
  heaps_i := heaps_i + 1;
end;
  heaps_res := [];
  heaps_res := concat(heaps_res, [copy_list(arr)]);
  heaps_i := 0;
  while heaps_i < heaps_n do begin
  if heaps_c[heaps_i] < heaps_i then begin
  if (heaps_i mod 2) = 0 then begin
  heaps_temp := arr[0];
  arr[0] := arr[heaps_i];
  arr[heaps_i] := heaps_temp;
end else begin
  heaps_temp := arr[heaps_c[heaps_i]];
  arr[heaps_c[heaps_i]] := arr[heaps_i];
  arr[heaps_i] := heaps_temp;
end;
  heaps_res := concat(heaps_res, [copy_list(arr)]);
  heaps_c[heaps_i] := heaps_c[heaps_i] + 1;
  heaps_i := 0;
end else begin
  heaps_c[heaps_i] := 0;
  heaps_i := heaps_i + 1;
end;
end;
  exit(heaps_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_list_int_to_str(heaps([1, 2, 3])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
