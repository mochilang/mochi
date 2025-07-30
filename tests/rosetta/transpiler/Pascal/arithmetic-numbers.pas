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
function sieve(limit: integer): IntArray; forward;
function primesFrom(spf: IntArray; limit: integer): IntArray; forward;
function pad3(n: integer): string; forward;
function commatize(n: integer): string; forward;
function primeCount(primes: IntArray; last: integer; spf: IntArray): integer; forward;
function arithmeticNumbers(limit: integer; spf: IntArray): IntArray; forward;
procedure main(); forward;
function sieve(limit: integer): IntArray;
var
  sieve_spf: array of integer;
  sieve_i: integer;
  sieve_j: integer;
begin
  sieve_spf := [];
  sieve_i := 0;
  while sieve_i <= limit do begin
  sieve_spf := concat(sieve_spf, [0]);
  sieve_i := sieve_i + 1;
end;
  sieve_i := 2;
  while sieve_i <= limit do begin
  if sieve_spf[sieve_i] = 0 then begin
  sieve_spf[sieve_i] := sieve_i;
  if (sieve_i * sieve_i) <= limit then begin
  sieve_j := sieve_i * sieve_i;
  while sieve_j <= limit do begin
  if sieve_spf[sieve_j] = 0 then begin
  sieve_spf[sieve_j] := sieve_i;
end;
  sieve_j := sieve_j + sieve_i;
end;
end;
end;
  sieve_i := sieve_i + 1;
end;
  exit(sieve_spf);
end;
function primesFrom(spf: IntArray; limit: integer): IntArray;
var
  primesFrom_primes: array of integer;
  primesFrom_i: integer;
begin
  primesFrom_primes := [];
  primesFrom_i := 3;
  while primesFrom_i <= limit do begin
  if spf[primesFrom_i] = primesFrom_i then begin
  primesFrom_primes := concat(primesFrom_primes, [primesFrom_i]);
end;
  primesFrom_i := primesFrom_i + 1;
end;
  exit(primesFrom_primes);
end;
function pad3(n: integer): string;
var
  pad3_s: string;
begin
  pad3_s := IntToStr(n);
  while Length(pad3_s) < 3 do begin
  pad3_s := ' ' + pad3_s;
end;
  exit(pad3_s);
end;
function commatize(n: integer): string;
var
  commatize_s: string;
  commatize_out: string;
  commatize_i: integer;
  commatize_c: integer;
begin
  commatize_s := IntToStr(n);
  commatize_out := '';
  commatize_i := Length(commatize_s) - 1;
  commatize_c := 0;
  while commatize_i >= 0 do begin
  commatize_out := copy(commatize_s, commatize_i+1, (commatize_i + 1 - (commatize_i))) + commatize_out;
  commatize_c := commatize_c + 1;
  if ((commatize_c mod 3) = 0) and (commatize_i > 0) then begin
  commatize_out := ',' + commatize_out;
end;
  commatize_i := commatize_i - 1;
end;
  exit(commatize_out);
end;
function primeCount(primes: IntArray; last: integer; spf: IntArray): integer;
var
  primeCount_lo: integer;
  primeCount_hi: integer;
  primeCount_mid: integer;
  primeCount_count: integer;
begin
  primeCount_lo := 0;
  primeCount_hi := Length(primes);
  while primeCount_lo < primeCount_hi do begin
  primeCount_mid := Trunc((primeCount_lo + primeCount_hi) div 2);
  if primes[primeCount_mid] < last then begin
  primeCount_lo := primeCount_mid + 1;
end else begin
  primeCount_hi := primeCount_mid;
end;
end;
  primeCount_count := primeCount_lo + 1;
  if spf[last] <> last then begin
  primeCount_count := primeCount_count - 1;
end;
  exit(primeCount_count);
end;
function arithmeticNumbers(limit: integer; spf: IntArray): IntArray;
var
  arithmeticNumbers_arr: array of integer;
  arithmeticNumbers_n: integer;
  arithmeticNumbers_x: integer;
  arithmeticNumbers_sigma: integer;
  arithmeticNumbers_tau: integer;
  arithmeticNumbers_p: integer;
  arithmeticNumbers_cnt: integer;
  arithmeticNumbers_power: integer;
  arithmeticNumbers_sum: integer;
begin
  arithmeticNumbers_arr := [1];
  arithmeticNumbers_n := 3;
  while Length(arithmeticNumbers_arr) < limit do begin
  if spf[arithmeticNumbers_n] = arithmeticNumbers_n then begin
  arithmeticNumbers_arr := concat(arithmeticNumbers_arr, [arithmeticNumbers_n]);
end else begin
  arithmeticNumbers_x := arithmeticNumbers_n;
  arithmeticNumbers_sigma := 1;
  arithmeticNumbers_tau := 1;
  while arithmeticNumbers_x > 1 do begin
  arithmeticNumbers_p := spf[arithmeticNumbers_x];
  if arithmeticNumbers_p = 0 then begin
  arithmeticNumbers_p := arithmeticNumbers_x;
end;
  arithmeticNumbers_cnt := 0;
  arithmeticNumbers_power := arithmeticNumbers_p;
  arithmeticNumbers_sum := 1;
  while (arithmeticNumbers_x mod arithmeticNumbers_p) = 0 do begin
  arithmeticNumbers_x := arithmeticNumbers_x div arithmeticNumbers_p;
  arithmeticNumbers_cnt := arithmeticNumbers_cnt + 1;
  arithmeticNumbers_sum := arithmeticNumbers_sum + arithmeticNumbers_power;
  arithmeticNumbers_power := arithmeticNumbers_power * arithmeticNumbers_p;
end;
  arithmeticNumbers_sigma := arithmeticNumbers_sigma * arithmeticNumbers_sum;
  arithmeticNumbers_tau := arithmeticNumbers_tau * (arithmeticNumbers_cnt + 1);
end;
  if (arithmeticNumbers_sigma mod arithmeticNumbers_tau) = 0 then begin
  arithmeticNumbers_arr := concat(arithmeticNumbers_arr, [arithmeticNumbers_n]);
end;
end;
  arithmeticNumbers_n := arithmeticNumbers_n + 1;
end;
  exit(arithmeticNumbers_arr);
end;
procedure main();
var
  main_limit: integer;
  main_spf: IntArray;
  main_primes: IntArray;
  main_arr: IntArray;
  main_i: integer;
  main_line: string;
  main_j: integer;
  main_x: integer;
  main_last: integer;
  main_lastc: string;
  main_pc: integer;
  main_comp: integer;
begin
  main_limit := 1228663;
  main_spf := sieve(main_limit);
  main_primes := primesFrom(main_spf, main_limit);
  main_arr := arithmeticNumbers(1000000, main_spf);
  writeln('The first 100 arithmetic numbers are:');
  main_i := 0;
  while main_i < 100 do begin
  main_line := '';
  main_j := 0;
  while main_j < 10 do begin
  main_line := main_line + pad3(main_arr[main_i + main_j]);
  if main_j < 9 then begin
  main_line := main_line + ' ';
end;
  main_j := main_j + 1;
end;
  writeln(main_line);
  main_i := main_i + 10;
end;
  for main_x in [1000, 10000, 100000, 1000000] do begin
  main_last := main_arr[main_x - 1];
  main_lastc := commatize(main_last);
  writeln((('' + #10 + 'The ' + commatize(main_x)) + 'th arithmetic number is: ') + main_lastc);
  main_pc := primeCount(main_primes, main_last, main_spf);
  main_comp := (main_x - main_pc) - 1;
  writeln(((('The count of such numbers <= ' + main_lastc) + ' which are composite is ') + commatize(main_comp)) + '.');
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
