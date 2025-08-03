{$mode objfpc}
program Main;
uses SysUtils;
type Int64Array = array of int64;
type Int64ArrayArray = array of Int64Array;
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
function padStart(s: string; l: integer; c: char): string;
var d: integer;
begin
  d := l - Length(s);
  if d > 0 then padStart := StringOfChar(c, d) + s else padStart := s;
end;
procedure show_list_int64(xs: array of int64);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function bellTriangle(n: integer): Int64ArrayArray; forward;
procedure main(); forward;
function bellTriangle(n: integer): Int64ArrayArray;
var
  bellTriangle_tri: array of Int64Array;
  bellTriangle_i: integer;
  bellTriangle_row: array of int64;
  bellTriangle_j: integer;
begin
  bellTriangle_tri := [];
  bellTriangle_i := 0;
  while bellTriangle_i < n do begin
  bellTriangle_row := [];
  bellTriangle_j := 0;
  while bellTriangle_j < bellTriangle_i do begin
  bellTriangle_row := concat(bellTriangle_row, [0]);
  bellTriangle_j := bellTriangle_j + 1;
end;
  bellTriangle_tri := concat(bellTriangle_tri, [bellTriangle_row]);
  bellTriangle_i := bellTriangle_i + 1;
end;
  bellTriangle_tri[1][0] := 1;
  bellTriangle_i := 2;
  while bellTriangle_i < n do begin
  bellTriangle_tri[bellTriangle_i][0] := bellTriangle_tri[bellTriangle_i - 1][bellTriangle_i - 2];
  bellTriangle_j := 1;
  while bellTriangle_j < bellTriangle_i do begin
  bellTriangle_tri[bellTriangle_i][bellTriangle_j] := bellTriangle_tri[bellTriangle_i][bellTriangle_j - 1] + bellTriangle_tri[bellTriangle_i - 1][bellTriangle_j - 1];
  bellTriangle_j := bellTriangle_j + 1;
end;
  bellTriangle_i := bellTriangle_i + 1;
end;
  exit(bellTriangle_tri);
end;
procedure main();
var
  main_bt: Int64ArrayArray;
  main_i: integer;
begin
  main_bt := bellTriangle(51);
  writeln('First fifteen and fiftieth Bell numbers:');
  for main_i := 1 to (16 - 1) do begin
  writeln((('' + padStart(IntToStr(main_i), 2, ' ')) + ': ') + IntToStr(main_bt[main_i][0]));
end;
  writeln('50: ' + IntToStr(main_bt[50][0]));
  writeln('');
  writeln('The first ten rows of Bell''s triangle:');
  for main_i := 1 to (11 - 1) do begin
  show_list_int64(main_bt[main_i]);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
