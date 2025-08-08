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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  k: integer;
  arr: IntArray;
  res: IntArrayArray;
function permute(k: integer; arr: IntArray; res: IntArrayArray): IntArrayArray; forward;
function heaps(arr: IntArray): IntArrayArray; forward;
procedure main(); forward;
function permute(k: integer; arr: IntArray; res: IntArrayArray): IntArrayArray;
var
  permute_copy: array of integer;
  permute_i: integer;
  permute_temp: integer;
begin
  if k = 1 then begin
  permute_copy := copy(arr, 0, Length(arr));
  exit(concat(res, [permute_copy]));
end;
  res := permute(k - 1, arr, res);
  permute_i := 0;
  while permute_i < (k - 1) do begin
  if (k mod 2) = 0 then begin
  permute_temp := arr[permute_i];
  arr[permute_i] := arr[k - 1];
  arr[k - 1] := permute_temp;
end else begin
  permute_temp := arr[0];
  arr[0] := arr[k - 1];
  arr[k - 1] := permute_temp;
end;
  res := permute(k - 1, arr, res);
  permute_i := permute_i + 1;
end;
  exit(res);
end;
function heaps(arr: IntArray): IntArrayArray;
var
  heaps_res: array of IntArray;
begin
  if Length(arr) <= 1 then begin
  exit([copy(arr, 0, Length(arr))]);
end;
  heaps_res := IntArrayArray([]);
  heaps_res := permute(Length(arr), arr, heaps_res);
  exit(heaps_res);
end;
procedure main();
var
  main_perms: IntArrayArray;
begin
  main_perms := heaps([1, 2, 3]);
  show_list_list(main_perms);
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
