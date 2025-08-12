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
  k: integer;
  n: integer;
function combinations(n: integer; k: integer): integer; forward;
function combinations(n: integer; k: integer): integer;
var
  combinations_res: integer;
  combinations_i: integer;
begin
  if (k < 0) or (n < k) then begin
  panic('Please enter positive integers for n and k where n >= k');
end;
  combinations_res := 1;
  combinations_i := 0;
  while combinations_i < k do begin
  combinations_res := combinations_res * (n - combinations_i);
  combinations_res := combinations_res div (combinations_i + 1);
  combinations_i := combinations_i + 1;
end;
  exit(combinations_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('The number of five-card hands possible from a standard fifty-two card deck is: ' + IntToStr(combinations(52, 5)));
  writeln('');
  writeln(('If a class of 40 students must be arranged into groups of 4 for group projects, there are ' + IntToStr(combinations(40, 4))) + ' ways to arrange them.');
  writeln('');
  writeln(('If 10 teams are competing in a Formula One race, there are ' + IntToStr(combinations(10, 3))) + ' ways that first, second and third place can be awarded.');
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.
