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
  pfacSum_sum: integer;
  pfacSum_p: integer;
  main_d: integer;
  main_a: integer;
  main_pnum: integer;
  main_i: integer;
  main_j: integer;
function pfacSum(i: integer): integer; forward;
procedure main(); forward;
function pfacSum(i: integer): integer;
begin
  pfacSum_sum := 0;
  pfacSum_p := 1;
  while pfacSum_p <= (i div 2) do begin
  if (i mod pfacSum_p) = 0 then begin
  pfacSum_sum := pfacSum_sum + pfacSum_p;
end;
  pfacSum_p := pfacSum_p + 1;
end;
  exit(pfacSum_sum);
end;
procedure main();
begin
  main_d := 0;
  main_a := 0;
  main_pnum := 0;
  main_i := 1;
  while main_i <= 20000 do begin
  main_j := pfacSum(main_i);
  if main_j < main_i then begin
  main_d := main_d + 1;
end;
  if main_j = main_i then begin
  main_pnum := main_pnum + 1;
end;
  if main_j > main_i then begin
  main_a := main_a + 1;
end;
  main_i := main_i + 1;
end;
  writeln(('There are ' + IntToStr(main_d)) + ' deficient numbers between 1 and 20000');
  writeln(('There are ' + IntToStr(main_a)) + ' abundant numbers  between 1 and 20000');
  writeln(('There are ' + IntToStr(main_pnum)) + ' perfect numbers between 1 and 20000');
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
