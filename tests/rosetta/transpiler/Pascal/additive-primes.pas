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
function sumDigits(n: integer): integer; forward;
function pad(n: integer): string; forward;
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
function sumDigits(n: integer): integer;
var
  sumDigits_s: integer;
  sumDigits_x: integer;
begin
  sumDigits_s := 0;
  sumDigits_x := n;
  while sumDigits_x > 0 do begin
  sumDigits_s := sumDigits_s + (sumDigits_x mod 10);
  sumDigits_x := Trunc(sumDigits_x div 10);
end;
  exit(sumDigits_s);
end;
function pad(n: integer): string;
begin
  if n < 10 then begin
  exit('  ' + IntToStr(n));
end;
  if n < 100 then begin
  exit(' ' + IntToStr(n));
end;
  exit(IntToStr(n));
end;
procedure main();
var
  main_count: integer;
  main_line: string;
  main_lineCount: integer;
  main_i: integer;
begin
  writeln('Additive primes less than 500:');
  main_count := 0;
  main_line := '';
  main_lineCount := 0;
  main_i := 2;
  while main_i < 500 do begin
  if isPrime(main_i) and isPrime(sumDigits(main_i)) then begin
  main_count := main_count + 1;
  main_line := (main_line + pad(main_i)) + '  ';
  main_lineCount := main_lineCount + 1;
  if main_lineCount = 10 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 2 - (0))));
  main_line := '';
  main_lineCount := 0;
end;
end;
  if main_i > 2 then begin
  main_i := main_i + 2;
end else begin
  main_i := main_i + 1;
end;
end;
  if main_lineCount > 0 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 2 - (0))));
end;
  writeln(IntToStr(main_count) + ' additive primes found.');
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
