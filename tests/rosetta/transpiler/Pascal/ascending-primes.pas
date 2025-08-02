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
  asc: array of integer;
function isPrime(n: integer): boolean; forward;
procedure gen(first: integer; cand: integer; digits: integer); forward;
function pad(n: integer; width: integer): string; forward;
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
procedure gen(first: integer; cand: integer; digits: integer);
var
  gen_i: integer;
begin
  if digits = 0 then begin
  if isPrime(cand) then begin
  asc := concat(asc, [cand]);
end;
  exit();
end;
  gen_i := first;
  while gen_i < 10 do begin
  gen(gen_i + 1, (cand * 10) + gen_i, digits - 1);
  gen_i := gen_i + 1;
end;
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
  main_digits: integer;
  main_i: integer;
  main_line: string;
begin
  main_digits := 1;
  while main_digits < 10 do begin
  gen(1, 0, main_digits);
  main_digits := main_digits + 1;
end;
  writeln(('There are ' + IntToStr(Length(asc))) + ' ascending primes, namely:');
  main_i := 0;
  main_line := '';
  while main_i < Length(asc) do begin
  main_line := (main_line + pad(asc[main_i], 8)) + ' ';
  if ((main_i + 1) mod 10) = 0 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 1 - (0))));
  main_line := '';
end;
  main_i := main_i + 1;
end;
  if Length(main_line) > 0 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 1 - (0))));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  asc := [];
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
