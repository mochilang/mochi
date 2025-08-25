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
function dp_count(dp_count_s: IntArray; dp_count_n: int64): int64; forward;
function dp_count(dp_count_s: IntArray; dp_count_n: int64): int64;
var
  dp_count_table: array of int64;
  dp_count_i: int64;
  dp_count_idx: int64;
  dp_count_coin_val: int64;
  dp_count_j: int64;
begin
  if dp_count_n < 0 then begin
  exit(0);
end;
  dp_count_table := [];
  dp_count_i := 0;
  while dp_count_i <= dp_count_n do begin
  dp_count_table := concat(dp_count_table, IntArray([0]));
  dp_count_i := dp_count_i + 1;
end;
  dp_count_table[0] := 1;
  dp_count_idx := 0;
  while dp_count_idx < Length(dp_count_s) do begin
  dp_count_coin_val := dp_count_s[dp_count_idx];
  dp_count_j := dp_count_coin_val;
  while dp_count_j <= dp_count_n do begin
  dp_count_table[dp_count_j] := dp_count_table[dp_count_j] + dp_count_table[dp_count_j - dp_count_coin_val];
  dp_count_j := dp_count_j + 1;
end;
  dp_count_idx := dp_count_idx + 1;
end;
  exit(dp_count_table[dp_count_n]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(dp_count([1, 2, 3], 4));
  writeln(dp_count([1, 2, 3], 7));
  writeln(dp_count([2, 5, 3, 6], 10));
  writeln(dp_count([10], 99));
  writeln(dp_count([4, 5, 6], 0));
  writeln(dp_count([1, 2, 3], -5));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.
