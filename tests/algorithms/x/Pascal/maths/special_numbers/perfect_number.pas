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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
function perfect(n: integer): boolean; forward;
procedure main(); forward;
function perfect(n: integer): boolean;
var
  perfect_limit: integer;
  perfect_sum: integer;
  perfect_i: integer;
begin
  if n <= 0 then begin
  exit(false);
end;
  perfect_limit := n div 2;
  perfect_sum := 0;
  perfect_i := 1;
  while perfect_i <= perfect_limit do begin
  if (n mod perfect_i) = 0 then begin
  perfect_sum := perfect_sum + perfect_i;
end;
  perfect_i := perfect_i + 1;
end;
  exit(perfect_sum = n);
end;
procedure main();
var
  main_numbers: array of integer;
  main_idx: integer;
  main_num: integer;
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
end.
