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
function shell_sort(shell_sort_collection: IntArray): IntArray; forward;
procedure main(); forward;
function shell_sort(shell_sort_collection: IntArray): IntArray;
var
  shell_sort_gap: int64;
  shell_sort_ten: int64;
  shell_sort_thirteen: int64;
  shell_sort_i: int64;
  shell_sort_temp: int64;
  shell_sort_j: int64;
begin
  shell_sort_gap := Length(shell_sort_collection);
  shell_sort_ten := 10;
  shell_sort_thirteen := 13;
  while shell_sort_gap > 1 do begin
  shell_sort_gap := (shell_sort_gap * shell_sort_ten) div shell_sort_thirteen;
  shell_sort_i := shell_sort_gap;
  while shell_sort_i < Length(shell_sort_collection) do begin
  shell_sort_temp := shell_sort_collection[shell_sort_i];
  shell_sort_j := shell_sort_i;
  while (shell_sort_j >= shell_sort_gap) and (shell_sort_collection[shell_sort_j - shell_sort_gap] > shell_sort_temp) do begin
  shell_sort_collection[shell_sort_j] := shell_sort_collection[shell_sort_j - shell_sort_gap];
  shell_sort_j := shell_sort_j - shell_sort_gap;
end;
  shell_sort_collection[shell_sort_j] := shell_sort_temp;
  shell_sort_i := shell_sort_i + 1;
end;
end;
  exit(shell_sort_collection);
end;
procedure main();
begin
  writeln(list_int_to_str(shell_sort([3, 2, 1])));
  writeln(list_int_to_str(shell_sort([])));
  writeln(list_int_to_str(shell_sort([1])));
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
  writeln('');
end.
