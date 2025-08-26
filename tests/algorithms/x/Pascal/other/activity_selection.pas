{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  start: array of int64;
  finish: array of int64;
procedure print_max_activities(print_max_activities_start: IntArray; print_max_activities_finish: IntArray); forward;
procedure print_max_activities(print_max_activities_start: IntArray; print_max_activities_finish: IntArray);
var
  print_max_activities_n: int64;
  print_max_activities_i: int64;
  print_max_activities_result_: string;
  print_max_activities_j: int64;
begin
  print_max_activities_n := Length(print_max_activities_finish);
  writeln('The following activities are selected:');
  print_max_activities_i := 0;
  print_max_activities_result_ := '0,';
  print_max_activities_j := 1;
  while print_max_activities_j < print_max_activities_n do begin
  if print_max_activities_start[print_max_activities_j] >= print_max_activities_finish[print_max_activities_i] then begin
  print_max_activities_result_ := (print_max_activities_result_ + IntToStr(print_max_activities_j)) + ',';
  print_max_activities_i := print_max_activities_j;
end;
  print_max_activities_j := print_max_activities_j + 1;
end;
  writeln(print_max_activities_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  start := [1, 3, 0, 5, 8, 5];
  finish := [2, 4, 6, 7, 9, 9];
  print_max_activities(start, finish);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
