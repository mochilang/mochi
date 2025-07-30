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
function pfacSum(i: integer): integer; forward;
function pad(n: integer; width: integer): string; forward;
procedure main(); forward;
function pfacSum(i: integer): integer;
var
  pfacSum_sum: integer;
  pfacSum_p: integer;
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
function pad(n: integer; width: integer): string;
var
  pad_s: string;
begin
  pad_s := IntToStr(n);
  while Length(pad_s) < width do begin
  pad_s := ' ' + pad_s;
end;
  exit(pad_s);
end;
procedure main();
var
  main_sums: array of integer;
  main_i: integer;
  main_n: integer;
  main_m: integer;
begin
  main_sums := [];
  main_i := 0;
  while main_i < 20000 do begin
  main_sums := concat(main_sums, [0]);
  main_i := main_i + 1;
end;
  main_i := 1;
  while main_i < 20000 do begin
  main_sums[main_i] := pfacSum(main_i);
  main_i := main_i + 1;
end;
  writeln('The amicable pairs below 20,000 are:');
  main_n := 2;
  while main_n < 19999 do begin
  main_m := main_sums[main_n];
  if ((main_m > main_n) and (main_m < 20000)) and (main_n = main_sums[main_m]) then begin
  writeln((('  ' + pad(main_n, 5)) + ' and ') + pad(main_m, 5));
end;
  main_n := main_n + 1;
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
