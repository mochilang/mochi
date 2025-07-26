{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
  gcd_x: integer;
  gcd_y: integer;
  gcd_t: integer;
  divisors_divs: array of integer;
  divisors_i: integer;
  divisors_j: integer;
  sum_s: integer;
  v: integer;
  isDuffinian_divs: IntArray;
  isDuffinian_sigma: integer;
  pad_s: string;
  printTable_i: integer;
  printTable_line: string;
  main_duff: array of integer;
  main_n: integer;
  main_triplets: array of string;
  main_i: integer;
  main_line: string;
  main_j: integer;
  padStr_res: string;
function gcd(a: integer; b: integer): integer; forward;
function divisors(n: integer): IntArray; forward;
function sum(xs: IntArray): integer; forward;
function isDuffinian(n: integer): boolean; forward;
function pad(n: integer; width: integer): string; forward;
procedure printTable(nums: IntArray; perRow: integer; width: integer); forward;
procedure main(); forward;
function padStr(s: string; width: integer): string; forward;
function gcd(a: integer; b: integer): integer;
begin
  gcd_x := a;
  if gcd_x < 0 then begin
  gcd_x := -gcd_x;
end;
  gcd_y := b;
  if gcd_y < 0 then begin
  gcd_y := -gcd_y;
end;
  while gcd_y <> 0 do begin
  gcd_t := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_t;
end;
  exit(gcd_x);
end;
function divisors(n: integer): IntArray;
begin
  divisors_divs := [];
  divisors_i := 1;
  while (divisors_i * divisors_i) <= n do begin
  if (n mod divisors_i) = 0 then begin
  divisors_divs := concat(divisors_divs, [divisors_i]);
  divisors_j := Trunc(n div divisors_i);
  if divisors_i <> divisors_j then begin
  divisors_divs := concat(divisors_divs, [divisors_j]);
end;
end;
  divisors_i := divisors_i + 1;
end;
  exit(divisors_divs);
end;
function sum(xs: IntArray): integer;
begin
  sum_s := 0;
  for v in xs do begin
  sum_s := sum_s + v;
end;
  exit(sum_s);
end;
function isDuffinian(n: integer): boolean;
begin
  isDuffinian_divs := divisors(n);
  if Length(isDuffinian_divs) <= 2 then begin
  exit(false);
end;
  isDuffinian_sigma := sum(isDuffinian_divs);
  exit(gcd(isDuffinian_sigma, n) = 1);
end;
function pad(n: integer; width: integer): string;
begin
  pad_s := IntToStr(n);
  while Length(pad_s) < width do begin
  pad_s := ' ' + pad_s;
end;
  exit(pad_s);
end;
procedure printTable(nums: IntArray; perRow: integer; width: integer);
begin
  printTable_i := 0;
  printTable_line := '';
  while printTable_i < Length(nums) do begin
  printTable_line := (printTable_line + ' ') + pad(nums[printTable_i], width);
  if ((printTable_i + 1) mod perRow) = 0 then begin
  writeln(copy(printTable_line, 1+1, (Length(printTable_line) - (1))));
  printTable_line := '';
end;
  printTable_i := printTable_i + 1;
end;
  if Length(printTable_line) > 0 then begin
  writeln(copy(printTable_line, 1+1, (Length(printTable_line) - (1))));
end;
end;
procedure main();
begin
  main_duff := [];
  main_n := 1;
  while Length(main_duff) < 50 do begin
  if isDuffinian(main_n) then begin
  main_duff := concat(main_duff, [main_n]);
end;
  main_n := main_n + 1;
end;
  writeln('First 50 Duffinian numbers:');
  printTable(main_duff, 10, 3);
  main_triplets := [];
  main_n := 1;
  while Length(main_triplets) < 20 do begin
  if (isDuffinian(main_n) and isDuffinian(main_n + 1)) and isDuffinian(main_n + 2) then begin
  main_triplets := concat(main_triplets, [((((('(' + IntToStr(main_n)) + ',') + IntToStr(main_n + 1)) + ',') + IntToStr(main_n + 2)) + ')']);
  main_n := main_n + 3;
end;
  main_n := main_n + 1;
end;
  writeln('' + #10 + 'First 20 Duffinian triplets:');
  main_i := 0;
  while main_i < Length(main_triplets) do begin
  main_line := '';
  main_j := 0;
  while (main_j < 4) and (main_i < Length(main_triplets)) do begin
  main_line := main_line + padStr(main_triplets[main_i], 16);
  main_j := main_j + 1;
  main_i := main_i + 1;
end;
  writeln(main_line);
end;
end;
function padStr(s: string; width: integer): string;
begin
  padStr_res := s;
  while Length(padStr_res) < width do begin
  padStr_res := padStr_res + ' ';
end;
  exit(padStr_res);
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
