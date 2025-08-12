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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
function sum_of_multiples(n: integer): integer; forward;
function sum_of_multiples(n: integer): integer;
var
  sum_of_multiples_total: integer;
  sum_of_multiples_terms: integer;
begin
  sum_of_multiples_total := 0;
  sum_of_multiples_terms := (n - 1) div 3;
  sum_of_multiples_total := sum_of_multiples_total + ((sum_of_multiples_terms * (6 + ((sum_of_multiples_terms - 1) * 3))) div 2);
  sum_of_multiples_terms := (n - 1) div 5;
  sum_of_multiples_total := sum_of_multiples_total + ((sum_of_multiples_terms * (10 + ((sum_of_multiples_terms - 1) * 5))) div 2);
  sum_of_multiples_terms := (n - 1) div 15;
  sum_of_multiples_total := sum_of_multiples_total - ((sum_of_multiples_terms * (30 + ((sum_of_multiples_terms - 1) * 15))) div 2);
  exit(sum_of_multiples_total);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('solution() = ' + IntToStr(sum_of_multiples(1000)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
