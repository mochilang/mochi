{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type BoolArray = array of boolean;
type BoolArrayArray = array of BoolArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function min_partitions(min_partitions_s: string): int64; forward;
function min_partitions(min_partitions_s: string): int64;
var
  min_partitions_n: integer;
  min_partitions_cut: array of int64;
  min_partitions_i: int64;
  min_partitions_pal: array of BoolArray;
  min_partitions_row: array of boolean;
  min_partitions_j: int64;
  min_partitions_mincut: int64;
  min_partitions_candidate: int64;
begin
  min_partitions_n := Length(min_partitions_s);
  min_partitions_cut := [];
  min_partitions_i := 0;
  while min_partitions_i < min_partitions_n do begin
  min_partitions_cut := concat(min_partitions_cut, IntArray([0]));
  min_partitions_i := min_partitions_i + 1;
end;
  min_partitions_pal := [];
  min_partitions_i := 0;
  while min_partitions_i < min_partitions_n do begin
  min_partitions_row := [];
  min_partitions_j := 0;
  while min_partitions_j < min_partitions_n do begin
  min_partitions_row := concat(min_partitions_row, [false]);
  min_partitions_j := min_partitions_j + 1;
end;
  min_partitions_pal := concat(min_partitions_pal, [min_partitions_row]);
  min_partitions_i := min_partitions_i + 1;
end;
  min_partitions_i := 0;
  while min_partitions_i < min_partitions_n do begin
  min_partitions_mincut := min_partitions_i;
  min_partitions_j := 0;
  while min_partitions_j <= min_partitions_i do begin
  if (min_partitions_s[min_partitions_i+1] = min_partitions_s[min_partitions_j+1]) and (((min_partitions_i - min_partitions_j) < 2) or min_partitions_pal[min_partitions_j + 1][min_partitions_i - 1]) then begin
  min_partitions_pal[min_partitions_j][min_partitions_i] := true;
  if min_partitions_j = 0 then begin
  min_partitions_mincut := 0;
end else begin
  min_partitions_candidate := min_partitions_cut[min_partitions_j - 1] + 1;
  if min_partitions_candidate < min_partitions_mincut then begin
  min_partitions_mincut := min_partitions_candidate;
end;
end;
end;
  min_partitions_j := min_partitions_j + 1;
end;
  min_partitions_cut[min_partitions_i] := min_partitions_mincut;
  min_partitions_i := min_partitions_i + 1;
end;
  exit(min_partitions_cut[min_partitions_n - 1]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(min_partitions('aab'));
  writeln(min_partitions('aaa'));
  writeln(min_partitions('ababbbabbababa'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
