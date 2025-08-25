{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function perfect(perfect_n: int64): boolean; forward;
procedure main(); forward;
function perfect(perfect_n: int64): boolean;
var
  perfect_limit: int64;
  perfect_sum: int64;
  perfect_i: int64;
begin
  if perfect_n <= 0 then begin
  exit(false);
end;
  perfect_limit := _floordiv(perfect_n, 2);
  perfect_sum := 0;
  perfect_i := 1;
  while perfect_i <= perfect_limit do begin
  if (perfect_n mod perfect_i) = 0 then begin
  perfect_sum := perfect_sum + perfect_i;
end;
  perfect_i := perfect_i + 1;
end;
  exit(perfect_sum = perfect_n);
end;
procedure main();
var
  main_numbers: array of int64;
  main_idx: int64;
  main_num: int64;
begin
  main_numbers := [6, 28, 29, 12, 496, 8128, 0, -1];
  main_idx := 0;
  while main_idx < Length(main_numbers) do begin
  main_num := main_numbers[main_idx];
  if perfect(main_num) then begin
  writeln(IntToStr(main_num) + ' is a Perfect Number.');
end else begin
  writeln(IntToStr(main_num) + ' is not a Perfect Number.');
end;
  main_idx := main_idx + 1;
end;
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
