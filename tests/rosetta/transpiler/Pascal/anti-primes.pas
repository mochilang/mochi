{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function countDivisors(n: integer): integer; forward;
procedure main(); forward;
function countDivisors(n: integer): integer;
var
  countDivisors_count: integer;
  countDivisors_i: integer;
begin
  if n < 2 then begin
  exit(1);
end;
  countDivisors_count := 2;
  countDivisors_i := 2;
  while countDivisors_i <= (n div 2) do begin
  if (n mod countDivisors_i) = 0 then begin
  countDivisors_count := countDivisors_count + 1;
end;
  countDivisors_i := countDivisors_i + 1;
end;
  exit(countDivisors_count);
end;
procedure main();
var
  main_maxDiv: integer;
  main_count: integer;
  main_n: integer;
  main_line: string;
  main_d: integer;
begin
  writeln('The first 20 anti-primes are:');
  main_maxDiv := 0;
  main_count := 0;
  main_n := 1;
  main_line := '';
  while main_count < 20 do begin
  main_d := countDivisors(main_n);
  if main_d > main_maxDiv then begin
  main_line := (main_line + IntToStr(main_n)) + ' ';
  main_maxDiv := main_d;
  main_count := main_count + 1;
end;
  main_n := main_n + 1;
end;
  main_line := copy(main_line, 0+1, (Length(main_line) - 1 - (0)));
  writeln(main_line);
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
