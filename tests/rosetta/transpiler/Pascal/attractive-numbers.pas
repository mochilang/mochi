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
function isPrime(n: integer): boolean; forward;
function countPrimeFactors(n: integer): integer; forward;
function pad4(n: integer): string; forward;
procedure main(); forward;
function isPrime(n: integer): boolean;
var
  isPrime_d: integer;
begin
  if n < 2 then begin
  exit(false);
end;
  if (n mod 2) = 0 then begin
  exit(n = 2);
end;
  if (n mod 3) = 0 then begin
  exit(n = 3);
end;
  isPrime_d := 5;
  while (isPrime_d * isPrime_d) <= n do begin
  if (n mod isPrime_d) = 0 then begin
  exit(false);
end;
  isPrime_d := isPrime_d + 2;
  if (n mod isPrime_d) = 0 then begin
  exit(false);
end;
  isPrime_d := isPrime_d + 4;
end;
  exit(true);
end;
function countPrimeFactors(n: integer): integer;
var
  countPrimeFactors_count: integer;
  countPrimeFactors_f: integer;
begin
  if n = 1 then begin
  exit(0);
end;
  if isPrime(n) then begin
  exit(1);
end;
  countPrimeFactors_count := 0;
  countPrimeFactors_f := 2;
  while true do begin
  if (n mod countPrimeFactors_f) = 0 then begin
  countPrimeFactors_count := countPrimeFactors_count + 1;
  n := n div countPrimeFactors_f;
  if n = 1 then begin
  exit(countPrimeFactors_count);
end;
  if isPrime(n) then begin
  countPrimeFactors_f := n;
end;
end else begin
  if countPrimeFactors_f >= 3 then begin
  countPrimeFactors_f := countPrimeFactors_f + 2;
end else begin
  countPrimeFactors_f := 3;
end;
end;
end;
  exit(countPrimeFactors_count);
end;
function pad4(n: integer): string;
var
  pad4_s: string;
begin
  pad4_s := IntToStr(n);
  while Length(pad4_s) < 4 do begin
  pad4_s := ' ' + pad4_s;
end;
  exit(pad4_s);
end;
procedure main();
var
  main_max: integer;
  main_count: integer;
  main_line: string;
  main_lineCount: integer;
  main_i: integer;
  main_c: integer;
begin
  main_max := 120;
  writeln(('The attractive numbers up to and including ' + IntToStr(main_max)) + ' are:');
  main_count := 0;
  main_line := '';
  main_lineCount := 0;
  main_i := 1;
  while main_i <= main_max do begin
  main_c := countPrimeFactors(main_i);
  if isPrime(main_c) then begin
  main_line := main_line + pad4(main_i);
  main_count := main_count + 1;
  main_lineCount := main_lineCount + 1;
  if main_lineCount = 20 then begin
  writeln(main_line);
  main_line := '';
  main_lineCount := 0;
end;
end;
  main_i := main_i + 1;
end;
  if main_lineCount > 0 then begin
  writeln(main_line);
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
